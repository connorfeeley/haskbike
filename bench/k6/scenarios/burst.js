// Burst scenario: ramp arrival rate up to find the knee of the latency
// curve. The output is most useful when watched live with --out console
// or when comparing the p95 trend across stages from the JSON summary.
//
// Usage:
//   BASE_URL=http://localhost:8081 \
//     k6 run --summary-export=results/burst.json bench/k6/scenarios/burst.js

import http from 'k6/http';
import { check } from 'k6';
import { BASE_URL, ENDPOINTS } from '../lib/params.js';

// Use the same weighted mix as steady but reach higher peak rates.
const TARGETS = ['data-station-status', 'data-events-docking', 'viz-system-status', 'viz-station-status'];
const ENDPOINT_BY_TAG = Object.fromEntries(ENDPOINTS.map((e) => [e.tag, e]));
const POOL = TARGETS.map((t) => ENDPOINT_BY_TAG[t]).filter(Boolean);

export const options = {
  scenarios: {
    burst: {
      executor: 'ramping-arrival-rate',
      startRate: 1,
      timeUnit: '1s',
      preAllocatedVUs: 20,
      maxVUs: 200,
      stages: [
        { duration: '30s', target: 5 },
        { duration: '30s', target: 10 },
        { duration: '30s', target: 25 },
        { duration: '30s', target: 50 },
        { duration: '30s', target: 100 },
        { duration: '30s', target: 0 },
      ],
    },
  },
  thresholds: {
    http_req_failed: ['rate<0.05'],
  },
};

export default function () {
  const ep = POOL[Math.floor(Math.random() * POOL.length)];
  const url = BASE_URL + ep.build(1);
  const res = http.get(url, { tags: { endpoint: ep.tag } });
  check(res, {
    'status 2xx': (r) => r.status >= 200 && r.status < 300,
  });
}
