// Steady-state scenario: constant arrival rate across a weighted mix of
// endpoints, sustained for a few minutes. The point is to find the p95
// latency under sustained load that resembles production traffic.
//
// Tune the rate via env var RATE (requests/sec, default 5) and duration
// via DURATION (default 5m). Higher rates require more pre-allocated VUs;
// k6 will scale up to MAX_VUS automatically.
//
// Usage:
//   BASE_URL=http://localhost:8081 RATE=10 DURATION=10m \
//     k6 run --summary-export=results/steady.json bench/k6/scenarios/steady.js

import http from 'k6/http';
import { check } from 'k6';
import { BASE_URL, ENDPOINTS } from '../lib/params.js';

const RATE = parseInt(__ENV.RATE || '5', 10);
const DURATION = __ENV.DURATION || '5m';
const PRE_ALLOCATED = parseInt(__ENV.PRE_ALLOCATED_VUS || '10', 10);
const MAX_VUS = parseInt(__ENV.MAX_VUS || '50', 10);

// Weights approximate expected production traffic. Edit to match your data.
// Visualization pages and a small set of data endpoints dominate.
const WEIGHTS = {
  'home':                          1,
  'viz-system-status':             8,
  'viz-station-status':            6,
  'viz-station-list':              3,
  'viz-station-occupancy':         3,
  'viz-system-information':        2,
  'viz-query-history':             1,
  'data-station-status':           6,
  'data-station-status-integral':  3,
  'data-station-status-factor':    2,
  'data-system-information':       3,
  'data-events-docking':           4,
  'data-events-charging':          2,
  'data-station-list':             3,
  'data-station-occupancy':        3,
  'data-performance-csv':          1,
  'comp-events-docking':           3,
  'comp-events-charging':          2,
  'comp-perf-station':             3,
  'comp-charging-infra':           2,
  'comp-latest-queries':           1,
};

const ENDPOINT_BY_TAG = Object.fromEntries(ENDPOINTS.map((e) => [e.tag, e]));
const WEIGHTED = (() => {
  const out = [];
  for (const tag of Object.keys(WEIGHTS)) {
    if (!ENDPOINT_BY_TAG[tag]) continue;
    for (let i = 0; i < WEIGHTS[tag]; i++) out.push(ENDPOINT_BY_TAG[tag]);
  }
  return out;
})();

export const options = {
  scenarios: {
    steady: {
      executor: 'constant-arrival-rate',
      rate: RATE,
      timeUnit: '1s',
      duration: DURATION,
      preAllocatedVUs: PRE_ALLOCATED,
      maxVUs: MAX_VUS,
    },
  },
  thresholds: {
    http_req_failed: ['rate<0.01'],
    http_req_duration: ['p(95)<5000'],
  },
};

export default function () {
  const ep = WEIGHTED[Math.floor(Math.random() * WEIGHTED.length)];
  // Spans are mostly 1-day; some fraction at 7d/30d to stress wider windows.
  const span = pickSpan();
  const url = BASE_URL + ep.build(span);
  const res = http.get(url, { tags: { endpoint: ep.tag, span: String(span) + 'd' } });
  check(res, {
    'status 2xx': (r) => r.status >= 200 && r.status < 300,
  });
}

function pickSpan() {
  const r = Math.random();
  if (r < 0.7) return 1;
  if (r < 0.9) return 7;
  return 30;
}
