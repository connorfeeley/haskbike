// Per-endpoint isolated scenario: hammer a single endpoint at a fixed rate.
// Use this to investigate a specific hotspot in isolation.
//
// Required env: ENDPOINT (one of the tags from lib/params.js)
// Optional env: RATE (req/s, default 5), DURATION (default 2m), SPAN_DAYS (default 1)
//
// Usage:
//   ENDPOINT=data-station-status-integral SPAN_DAYS=7 RATE=2 DURATION=2m \
//     k6 run --summary-export=results/single.json bench/k6/scenarios/per-endpoint/single.js

import http from 'k6/http';
import { check } from 'k6';
import { BASE_URL, ENDPOINTS } from '../../lib/params.js';

const ENDPOINT_TAG = __ENV.ENDPOINT;
if (!ENDPOINT_TAG) {
  throw new Error('ENDPOINT env var is required (e.g. ENDPOINT=data-station-status)');
}
const EP = ENDPOINTS.find((e) => e.tag === ENDPOINT_TAG);
if (!EP) {
  throw new Error(`Unknown ENDPOINT '${ENDPOINT_TAG}'; see bench/k6/lib/params.js for tags`);
}

const RATE = parseInt(__ENV.RATE || '5', 10);
const DURATION = __ENV.DURATION || '2m';
const SPAN_DAYS = parseInt(__ENV.SPAN_DAYS || '1', 10);

export const options = {
  scenarios: {
    single: {
      executor: 'constant-arrival-rate',
      rate: RATE,
      timeUnit: '1s',
      duration: DURATION,
      preAllocatedVUs: 10,
      maxVUs: 50,
    },
  },
  thresholds: {
    http_req_failed: ['rate<0.01'],
  },
};

export default function () {
  const url = BASE_URL + EP.build(SPAN_DAYS);
  const res = http.get(url, { tags: { endpoint: EP.tag, span: SPAN_DAYS + 'd' } });
  check(res, {
    'status 2xx': (r) => r.status >= 200 && r.status < 300,
  });
}
