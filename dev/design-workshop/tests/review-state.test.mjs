import test from "node:test";
import assert from "node:assert/strict";

import {
  buildReviewSummary,
  createEmptyReview,
  criteria,
  directionIds,
  normalizeReview
} from "../lib/review.js";

test("empty review contains every criterion for every direction", () => {
  const review = createEmptyReview();

  assert.equal(review.version, 2);
  assert.equal(directionIds.length, 4);
  assert.deepEqual(Object.keys(review.ratings["circadian-field"]), criteria);
  assert.deepEqual(Object.keys(review.ratings["clear-measure"]), criteria);
  assert.equal(review.preferredDirection, "");
});

test("normalization rejects unknown values and preserves valid feedback", () => {
  const review = createEmptyReview();
  review.ratings["circadian-field"][criteria[0]] = "Strong";
  review.ratings["spectral-console"][criteria[0]] = "Excellent";
  review.preferredDirection = "circadian-field";

  const clean = normalizeReview(review);

  assert.equal(clean.ratings["circadian-field"][criteria[0]], "Strong");
  assert.equal(clean.ratings["spectral-console"][criteria[0]], "");
  assert.equal(clean.preferredDirection, "circadian-field");
});

test("summary includes decision, ratings, notes, and borrowed elements", () => {
  const review = createEmptyReview();
  review.preferredDirection = "circadian-field";
  review.ratings["circadian-field"][criteria[0]] = "Strong";
  review.notes["circadian-field"] = "Best balance.";
  review.borrowedElements = "Use the console table density.";

  const summary = buildReviewSummary(review);

  assert.match(summary, /Preferred direction: Circadian Field/);
  assert.match(summary, /Scientific credibility: Strong/);
  assert.match(summary, /Notes: Best balance\./);
  assert.match(summary, /Clear Measure/);
  assert.match(summary, /Elements to borrow: Use the console table density\./);
});

test("normalization resets review objects from the previous schema", () => {
  const previous = createEmptyReview();
  previous.version = 1;
  previous.preferredDirection = "circadian-field";

  const clean = normalizeReview(previous);

  assert.equal(clean.version, 2);
  assert.equal(clean.preferredDirection, "");
});
