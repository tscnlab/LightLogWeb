export const REVIEW_VERSION = 2;
export const REVIEW_STORAGE_KEY = "lightlogweb.designReview.v2";
export const THEME_STORAGE_KEY = "lightlogweb.designWorkshop.theme.v1";

export const directionIds = [
  "circadian-field",
  "spectral-console",
  "daylight-almanac",
  "clear-measure"
];

export const directionNames = {
  "circadian-field": "Circadian Field",
  "spectral-console": "Spectral Console",
  "daylight-almanac": "Daylight Almanac",
  "clear-measure": "Clear Measure"
};

export const criteria = [
  "Scientific credibility",
  "Beginner approachability",
  "LightLogR kinship",
  "Distinctiveness",
  "Data-visualization fit",
  "Accessibility",
  "bslib feasibility"
];

const validRatings = new Set(["", "Strong", "Mixed", "Weak"]);

export function createEmptyReview() {
  return {
    version: REVIEW_VERSION,
    ratings: Object.fromEntries(
      directionIds.map((directionId) => [
        directionId,
        Object.fromEntries(criteria.map((criterion) => [criterion, ""]))
      ])
    ),
    notes: Object.fromEntries(directionIds.map((directionId) => [directionId, ""])),
    preferredDirection: "",
    borrowedElements: ""
  };
}

export function normalizeReview(candidate) {
  const clean = createEmptyReview();

  if (!candidate || candidate.version !== REVIEW_VERSION) {
    return clean;
  }

  for (const directionId of directionIds) {
    for (const criterion of criteria) {
      const rating = candidate.ratings?.[directionId]?.[criterion];
      clean.ratings[directionId][criterion] = validRatings.has(rating) ? rating : "";
    }
    clean.notes[directionId] = String(candidate.notes?.[directionId] ?? "").slice(0, 4000);
  }

  clean.preferredDirection = directionIds.includes(candidate.preferredDirection)
    ? candidate.preferredDirection
    : "";
  clean.borrowedElements = String(candidate.borrowedElements ?? "").slice(0, 4000);

  return clean;
}

export function buildReviewSummary(review) {
  const clean = normalizeReview(review);
  const lines = [
    "LightLogWeb visual-direction review",
    `Preferred direction: ${clean.preferredDirection ? directionNames[clean.preferredDirection] : "Not selected"}`,
    ""
  ];

  for (const directionId of directionIds) {
    lines.push(directionNames[directionId]);
    for (const criterion of criteria) {
      lines.push(`- ${criterion}: ${clean.ratings[directionId][criterion] || "Not rated"}`);
    }
    lines.push(`Notes: ${clean.notes[directionId].trim() || "None"}`, "");
  }

  lines.push(`Elements to borrow: ${clean.borrowedElements.trim() || "None"}`);
  return lines.join("\n");
}
