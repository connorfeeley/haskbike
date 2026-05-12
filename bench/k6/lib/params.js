// Shared helpers for haskbike k6 scenarios.
//
// Exposes:
//   - BASE_URL: target server, override with env var BASE_URL.
//   - STATION_IDS: pool of station IDs to sample from. Override via env var
//     STATION_IDS as a comma-separated list. The defaults are stations
//     known to exist in the Toronto Bike Share dataset; if you mirror a
//     different dataset, replace these.
//   - randStation(): pick a random station ID.
//   - randTimeRange(spanDays, refDate): produce {start, end} formatted as
//     ISO LocalTime (no timezone), which is what the haskbike server's
//     `LocalTime` query parameters expect.
//   - qs(obj): build a URL query string from an object, skipping undefined.
//
// Default reference date is 2024-01-01T00:00:00. Override via REF_DATE
// env var (ISO LocalTime) — set this to a date your mirrored DB has data
// for, AND match it to the anchor you pass to `just warm-occupancy
// --end <ts>` so warmed buckets and bench-requested buckets coincide
// (otherwise the now-anchored portion of the bench refills from scratch
// instead of hitting the warm cache).
//
// Production-realistic measurement:
//   REF_DATE=2024-01-01T00:00:00 just warm-occupancy --end "2024-01-01 00:00:00 UTC"
//   REF_DATE=2024-01-01T00:00:00 just bench-server

import { randomItem } from 'https://jslib.k6.io/k6-utils/1.4.0/index.js';

export const BASE_URL = __ENV.BASE_URL || 'http://localhost:8081';

const DEFAULT_STATION_IDS = [
  7001, 7002, 7003, 7004, 7005, 7006, 7007, 7008, 7009, 7010,
  7011, 7012, 7013, 7014, 7015, 7016, 7017, 7018, 7019, 7020,
];

export const STATION_IDS = (__ENV.STATION_IDS
  ? __ENV.STATION_IDS.split(',').map((s) => parseInt(s.trim(), 10))
  : DEFAULT_STATION_IDS
).filter((n) => Number.isFinite(n));

const REF_DATE = new Date(__ENV.REF_DATE || '2024-01-01T00:00:00');

export function randStation() {
  return randomItem(STATION_IDS);
}

// Format a Date as ISO LocalTime (no timezone), matching what the server's
// LocalTime parameter parser expects, e.g. "2024-01-01T00:00:00".
function fmtLocal(d) {
  const pad = (n) => String(n).padStart(2, '0');
  return (
    d.getUTCFullYear() +
    '-' + pad(d.getUTCMonth() + 1) +
    '-' + pad(d.getUTCDate()) +
    'T' + pad(d.getUTCHours()) +
    ':' + pad(d.getUTCMinutes()) +
    ':' + pad(d.getUTCSeconds())
  );
}

// Pick a random window of `spanDays` ending at REF_DATE. The start date
// is uniformly distributed in [REF_DATE - 90 days - spanDays, REF_DATE - spanDays]
// so we exercise different windows each call. This is a stress test for
// caches keyed on `(start, end)` — the wide horizon makes hits unlikely.
export function randTimeRange(spanDays) {
  const horizonMs = 90 * 24 * 60 * 60 * 1000;
  const spanMs = spanDays * 24 * 60 * 60 * 1000;
  const offset = Math.random() * horizonMs;
  const end = new Date(REF_DATE.getTime() - offset);
  const start = new Date(end.getTime() - spanMs);
  return { start: fmtLocal(start), end: fmtLocal(end) };
}

// "Production-like" range: a window of `spanDays` ending at REF_DATE,
// with a small random jitter (up to a few minutes) to simulate real users
// hitting the dashboard within a short window of one another. Concurrent
// users land on the same hour bucket after server-side rounding, which is
// what `queryStationOccupancyBucketed` is designed to exploit.
export function nowAnchoredRange(spanDays) {
  const jitterMs = Math.random() * 5 * 60 * 1000; // up to 5 minutes
  const end = new Date(REF_DATE.getTime() - jitterMs);
  const start = new Date(end.getTime() - spanDays * 24 * 60 * 60 * 1000);
  return { start: fmtLocal(start), end: fmtLocal(end) };
}

export function qs(obj) {
  const parts = [];
  for (const k of Object.keys(obj)) {
    const v = obj[k];
    if (v === undefined || v === null) continue;
    parts.push(encodeURIComponent(k) + '=' + encodeURIComponent(v));
  }
  return parts.length ? '?' + parts.join('&') : '';
}

// All endpoints we exercise, paired with a function that builds the URL
// from random params. Each entry has:
//   tag: short string for k6 url tagging
//   build(spanDays): returns a path with query string
export const ENDPOINTS = [
  // Home / static / cheap
  { tag: 'home',                build: () => '/' },
  { tag: 'debug-version',       build: () => '/debug/version' },
  { tag: 'robots',              build: () => '/robots.txt' },

  // Data API
  { tag: 'data-station-status',          build: (d) => '/data/station-status' + qs({ 'station-id': randStation(), ...rangeKeys(d) }) },
  { tag: 'data-station-status-integral', build: (d) => '/data/station-status/integral' + qs({ 'station-id': randStation(), ...rangeKeys(d) }) },
  { tag: 'data-station-status-factor',   build: (d) => '/data/station-status/factor' + qs({ 'station-id': randStation(), ...rangeKeys(d) }) },
  { tag: 'data-system-information',      build: (d) => '/data/system-information' + qs(rangeKeys(d)) },
  { tag: 'data-performance-csv',         build: (d) => '/data/system-status/performance/csv' + qs({ 'station-id': randStation(), ...rangeKeys(d) }) },
  { tag: 'data-events-docking',          build: (d) => '/data/events/docking' + qs({ 'station-id': randStation(), ...rangeKeys(d) }) },
  { tag: 'data-events-charging',         build: (d) => '/data/events/charging' + qs({ 'station-id': randStation(), ...rangeKeys(d) }) },
  { tag: 'data-station-list',            build: ()  => '/data/station-list' + qs({ time: randTimeRange(0).end }) },
  { tag: 'data-station-occupancy',       build: (d) => '/data/station-occupancy' + qs(rangeKeys(d)) },

  // Visualization (HTML pages)
  { tag: 'viz-station-status',           build: ()  => '/visualization/station-status' + qs({ 'station-id': randStation() }) },
  { tag: 'viz-system-status',            build: (d) => '/visualization/system-status' + qs(rangeKeys(d)) },
  { tag: 'viz-station-list',             build: ()  => '/visualization/station-list' },
  { tag: 'viz-station-occupancy',        build: (d) => '/visualization/station-occupancy' + qs(rangeKeys(d)) },
  { tag: 'viz-system-information',       build: ()  => '/visualization/system-information' },
  { tag: 'viz-performance-csv',          build: ()  => '/visualization/system-status/performance/csv' },
  { tag: 'viz-query-history',            build: ()  => '/visualization/query-history' },

  // Components (HTMX fragments)
  { tag: 'comp-events-docking',          build: (d) => '/components/events/docking' + qs({ 'station-id': randStation(), ...rangeKeys(d) }) },
  { tag: 'comp-events-charging',         build: (d) => '/components/events/charging' + qs({ 'station-id': randStation(), ...rangeKeys(d) }) },
  { tag: 'comp-charging-infra',          build: ()  => '/components/system-status/charging-infrastructure' + qs({ time: randTimeRange(0).end }) },
  { tag: 'comp-perf-station',            build: (d) => '/components/station-status/performance/' + randStation() + qs(rangeKeys(d)) },
  { tag: 'comp-latest-queries',          build: ()  => '/components/latest-queries' },
];

// Default range generator. Set ANCHOR_MODE=now in the env to use
// production-like now-anchored ranges (cache-friendly); set
// ANCHOR_MODE=random for the stress-test path. Default is "mixed":
// 70 % now-anchored, 30 % random — close to what real traffic looks like.
function rangeKeys(spanDays) {
  const days = spanDays || 1;
  const mode = (__ENV.ANCHOR_MODE || 'mixed').toLowerCase();
  let r;
  if (mode === 'now') {
    r = nowAnchoredRange(days);
  } else if (mode === 'random') {
    r = randTimeRange(days);
  } else {
    r = Math.random() < 0.7 ? nowAnchoredRange(days) : randTimeRange(days);
  }
  return { 'start-time': r.start, 'end-time': r.end };
}
