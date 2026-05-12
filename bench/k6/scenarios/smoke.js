// Smoke scenario: hit every endpoint once at low rate. Used to confirm
// every route returns 2xx and that request IDs land in server logs.
//
// Usage:
//   BASE_URL=http://localhost:8081 k6 run bench/k6/scenarios/smoke.js
//
// Pass --summary-export=path/to/summary.json to capture the full results.

import http from 'k6/http';
import { check } from 'k6';
import { BASE_URL, ENDPOINTS } from '../lib/params.js';

export const options = {
  scenarios: {
    smoke: {
      executor: 'shared-iterations',
      vus: 1,
      iterations: ENDPOINTS.length,
      maxDuration: '60s',
    },
  },
  thresholds: {
    http_req_failed: ['rate<0.01'],
  },
};

export default function () {
  // __ITER is the current iteration index for this VU.
  const ep = ENDPOINTS[__ITER % ENDPOINTS.length];
  const url = BASE_URL + ep.build(1);
  const res = http.get(url, { tags: { endpoint: ep.tag } });
  check(res, {
    [`${ep.tag}: status 2xx`]: (r) => r.status >= 200 && r.status < 300,
  });
}
