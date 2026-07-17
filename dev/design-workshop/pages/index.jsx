import Head from "next/head";
import { useEffect, useMemo, useState } from "react";

import {
  REVIEW_STORAGE_KEY,
  THEME_STORAGE_KEY,
  buildReviewSummary,
  createEmptyReview,
  criteria,
  directionIds,
  directionNames,
  normalizeReview
} from "../lib/review.js";

const directions = [
  {
    id: "circadian-field",
    name: "Circadian Field",
    number: "01",
    recommended: true,
    className: "direction--circadian",
    image: "/mood-circadian-field.png",
    imageAlt:
      "Abstract deep blue-green dawn field with a luminous cyan circadian arc and restrained amber horizon",
    promise: "A calm analytical field where personal light becomes legible across time.",
    description:
      "Scientific without feeling clinical. A quiet daylight field, explicit hierarchy, and restrained time-based flourishes keep consequential choices visible while leaving plots and tables in charge.",
    keywords: ["Calm", "Temporal", "Trustworthy", "Open"],
    typography: "Source Sans 3 + Source Serif 4",
    typeDetail:
      "Source Sans 3 carries controls, tables, and body copy. Source Serif 4 appears only in short narrative headings and decision explanations.",
    palette: [
      { role: "Ink", value: "#102a33" },
      { role: "Daylight", value: "#f6f8f4" },
      { role: "Cyan", value: "#0b6675" },
      { role: "Amber", value: "#c36a1d" },
      { role: "Night", value: "#081a20" }
    ],
    strengths: [
      "Balances expert credibility with a welcoming first-use experience.",
      "Provides a neutral stage for dense plots, tables, and status messages.",
      "Translates cleanly into bslib semantic tokens and restrained Sass rules."
    ],
    accessibility:
      "High-contrast ink and daylight surfaces establish the base. Cyan and amber are emphasis channels only; every status keeps a symbol and text label.",
    tradeoff:
      "Its restraint needs disciplined art direction. Too little temporal structure could make it feel like generic research software."
  },
  {
    id: "spectral-console",
    name: "Spectral Console",
    number: "02",
    recommended: false,
    className: "direction--spectral",
    image: "/mood-spectral-console.png",
    imageAlt:
      "Abstract graphite measurement field with electric-blue and violet traces crossing precise geometric planes",
    promise: "An instrument-grade console for exacting, data-dense light analysis.",
    description:
      "Crisp geometry, measured spacing, and a technical typographic voice foreground precision. Spectral accents behave like calibrated signals rather than decoration.",
    keywords: ["Precise", "Dense", "Measured", "Expert"],
    typography: "IBM Plex Sans + IBM Plex Mono",
    typeDetail:
      "IBM Plex Sans handles interface text. IBM Plex Mono is reserved for timestamps, units, revisions, and diagnostic values—not general body copy.",
    palette: [
      { role: "Graphite", value: "#111827" },
      { role: "Mist", value: "#f4f6f8" },
      { role: "Signal", value: "#174ea6" },
      { role: "Violet", value: "#7048d8" },
      { role: "Console", value: "#0f1218" }
    ],
    strengths: [
      "Makes dense tables, diagnostics, and revisions feel native.",
      "Creates the strongest distinction from consumer wellness products.",
      "Supports precise alignment and compact expert workflows."
    ],
    accessibility:
      "Mist and graphite provide strong base contrast. Blue and violet are paired with line style, symbols, and labels; mono text is limited to short values.",
    tradeoff:
      "The technical tone can raise the perceived learning barrier and may make recoverable warnings feel more severe than intended."
  },
  {
    id: "daylight-almanac",
    name: "Daylight Almanac",
    number: "03",
    recommended: false,
    className: "direction--almanac",
    image: "/mood-daylight-almanac.png",
    imageAlt:
      "Warm paper collage moving from coral dawn to indigo dusk along a gentle sun-path",
    promise: "A humane field guide for understanding light, days, and analysis decisions.",
    description:
      "Warm mineral surfaces and editorial pacing turn the workflow into an explanatory record. The interface feels patient and reflective without becoming a lifestyle product.",
    keywords: ["Human", "Editorial", "Warm", "Explanatory"],
    typography: "Atkinson Hyperlegible Next + Source Serif 4",
    typeDetail:
      "Atkinson Hyperlegible Next prioritizes recognition in controls and data labels. Source Serif 4 lends short explanations an almanac-like editorial rhythm.",
    palette: [
      { role: "Ink", value: "#2a2637" },
      { role: "Paper", value: "#fbf7ef" },
      { role: "Indigo", value: "#455491" },
      { role: "Coral", value: "#a74736" },
      { role: "Dusk", value: "#1d1a24" }
    ],
    strengths: [
      "Makes complex explanations and onboarding feel unusually approachable.",
      "Connects naturally to days, diaries, and longitudinal observation.",
      "Creates a distinctive voice for narrative reports and empty states."
    ],
    accessibility:
      "Hyperlegible body text and warm high-contrast surfaces support sustained reading. Coral and indigo always retain text or shape reinforcement.",
    tradeoff:
      "The editorial warmth can compete with dense analytical output and risks feeling less like a rigorous instrument."
  },
  {
    id: "clear-measure",
    name: "Clear Measure",
    number: "04",
    recommended: false,
    className: "direction--clear",
    image: "/mood-clear-measure.png",
    imageAlt:
      "Minimal off-white architectural field crossed by one precise cobalt band and fine slate alignment rules",
    promise: "A minimal professional system that makes rigor feel effortless.",
    description:
      "Disciplined whitespace, one blue emphasis channel, and quiet modernist typography remove almost every nonessential gesture. The result feels established and professional while keeping scientific content unmistakably primary.",
    keywords: ["Minimal", "Ordered", "Neutral", "Professional"],
    typography: "Source Sans 3",
    typeDetail:
      "One sans-serif family carries the entire system. Weight, scale, spacing, and tabular numerals establish hierarchy without a secondary display voice.",
    palette: [
      { role: "Ink", value: "#17232b" },
      { role: "Paper", value: "#f8fafb" },
      { role: "Cobalt", value: "#255b8f" },
      { role: "Steel", value: "#56656e" },
      { role: "Night", value: "#10171c" }
    ],
    strengths: [
      "Creates the clearest professional baseline with very little visual friction.",
      "Leaves maximum room for plots, tables, forms, and long analytical sessions.",
      "Maps to bslib with the fewest custom rules and the lowest maintenance risk."
    ],
    accessibility:
      "A high-contrast neutral foundation and one restrained blue emphasis channel reduce ambiguity. Statuses still retain distinct symbols, labels, and semantic colors.",
    tradeoff:
      "The deliberate neutrality is the least ownable territory. Without careful proportions and the hex measure mark, it could resemble many professional data products."
  }
];

function HexMark({ directionId, size = "large" }) {
  const titleId = `${directionId}-mark-title-${size}`;
  const clipId = `${directionId}-mark-clip-${size}`;

  if (directionId === "spectral-console") {
    return (
      <svg className={`hex-mark hex-mark--${size}`} viewBox="0 0 160 176" role="img" aria-labelledby={titleId}>
        <title id={titleId}>Spectral Console hex prism concept</title>
        <polygon className="hex-mark__surface" points="80,6 150,47 150,129 80,170 10,129 10,47" />
        <polygon className="hex-mark__outline" points="80,6 150,47 150,129 80,170 10,129 10,47" />
        <path className="hex-mark__beam" d="M22 120 L139 47" />
        <path className="hex-mark__beam hex-mark__beam--secondary" d="M28 133 L145 60" />
        {[38, 52, 66, 80, 94, 108, 122].map((x, index) => (
          <path key={x} className="hex-mark__tick" d={`M${x} ${129 - index * 8} l5 8`} />
        ))}
      </svg>
    );
  }

  if (directionId === "daylight-almanac") {
    return (
      <svg className={`hex-mark hex-mark--${size}`} viewBox="0 0 160 176" role="img" aria-labelledby={titleId}>
        <title id={titleId}>Daylight Almanac hex sun-path concept</title>
        <defs>
          <clipPath id={clipId}>
            <polygon points="80,6 150,47 150,129 80,170 10,129 10,47" />
          </clipPath>
        </defs>
        <g clipPath={`url(#${clipId})`}>
          <rect className="hex-mark__surface" x="0" y="0" width="160" height="176" />
          <path className="hex-mark__night" d="M0 105 Q55 88 160 102 L160 176 L0 176 Z" />
          <path className="hex-mark__horizon" d="M4 104 Q76 88 156 103" />
          <path className="hex-mark__sun-path" d="M24 106 Q78 23 136 106" />
          <circle className="hex-mark__sun" cx="80" cy="51" r="8" />
        </g>
        <polygon className="hex-mark__outline" points="80,6 150,47 150,129 80,170 10,129 10,47" />
      </svg>
    );
  }

  if (directionId === "clear-measure") {
    return (
      <svg className={`hex-mark hex-mark--${size}`} viewBox="0 0 160 176" role="img" aria-labelledby={titleId}>
        <title id={titleId}>Clear Measure hex gauge concept</title>
        <polygon className="hex-mark__surface" points="80,6 150,47 150,129 80,170 10,129 10,47" />
        <path className="hex-mark__measure" d="M22 88 H138" />
        <path className="hex-mark__notch" d="M80 75 V101" />
        <polygon className="hex-mark__outline" points="80,6 150,47 150,129 80,170 10,129 10,47" />
      </svg>
    );
  }

  return (
    <svg className={`hex-mark hex-mark--${size}`} viewBox="0 0 160 176" role="img" aria-labelledby={titleId}>
      <title id={titleId}>Circadian Field hex aperture and cycle concept</title>
      <defs>
        <clipPath id={clipId}>
          <polygon points="80,6 150,47 150,129 80,170 10,129 10,47" />
        </clipPath>
      </defs>
      <polygon className="hex-mark__surface" points="80,6 150,47 150,129 80,170 10,129 10,47" />
      <g clipPath={`url(#${clipId})`}>
        <path className="hex-mark__field" d="M-5 131 Q65 101 166 115 L166 183 L-5 183 Z" />
        <path className="hex-mark__sun-path" d="M17 126 Q78 27 145 116" />
        <circle className="hex-mark__aperture" cx="80" cy="55" r="7" />
      </g>
      <polygon className="hex-mark__outline" points="80,6 150,47 150,129 80,170 10,129 10,47" />
    </svg>
  );
}

function MiniChart({ directionId }) {
  const titleId = `${directionId}-plot-title`;
  const descId = `${directionId}-plot-desc`;

  return (
    <svg className="mini-chart" viewBox="0 0 520 210" role="img" aria-labelledby={`${titleId} ${descId}`}>
      <title id={titleId}>Illustrative personal light exposure plot</title>
      <desc id={descId}>
        A line rises from low morning exposure to a midday peak, falls in the afternoon, and ends near zero at night.
      </desc>
      <g className="mini-chart__grid">
        <path d="M52 28 H500" />
        <path d="M52 77 H500" />
        <path d="M52 126 H500" />
        <path d="M52 175 H500" />
        <path d="M52 28 V175" />
      </g>
      <path className="mini-chart__day-band" d="M52 175 L52 124 Q146 72 238 48 Q340 26 500 105 L500 175 Z" />
      <path
        className="mini-chart__line"
        d="M52 164 C86 162 107 148 132 125 S176 76 204 82 S247 36 282 43 S322 74 352 70 S404 100 430 123 S469 155 500 159"
      />
      {[
        [52, 164],
        [132, 125],
        [204, 82],
        [282, 43],
        [352, 70],
        [430, 123],
        [500, 159]
      ].map(([cx, cy]) => (
        <circle key={`${cx}-${cy}`} className="mini-chart__point" cx={cx} cy={cy} r="4" />
      ))}
      <g className="mini-chart__labels">
        <text x="52" y="198">00:00</text>
        <text x="164" y="198">06:00</text>
        <text x="276" y="198">12:00</text>
        <text x="388" y="198">18:00</text>
        <text x="500" y="198" textAnchor="end">24:00</text>
        <text x="42" y="35" textAnchor="end">1k</text>
        <text x="42" y="179" textAnchor="end">0</text>
      </g>
    </svg>
  );
}

function StatusChip({ symbol, label, kind }) {
  return (
    <span className={`status-chip status-chip--${kind}`}>
      <span aria-hidden="true">{symbol}</span>
      <span>{label}</span>
    </span>
  );
}

function ComponentSpecimen({ direction }) {
  const [applied, setApplied] = useState(false);

  return (
    <div className="component-specimen" aria-label={`${direction.name} bslib-compatible component specimen`}>
      <div className="specimen-topbar">
        <span className="specimen-brand">
          <HexMark directionId={direction.id} size="small" />
          <span>LightLogWeb</span>
        </span>
        <span className="specimen-context">Dataset · Participant week</span>
      </div>

      <div className="specimen-nav" aria-label="Illustrative navigation, not final information architecture">
        <span className="specimen-nav__item specimen-nav__item--active">Inspect</span>
        <span className="specimen-nav__item">Prepare</span>
        <span className="specimen-nav__item">Analyse</span>
      </div>

      <div className="specimen-layout">
        <section className="specimen-plot" aria-labelledby={`${direction.id}-plot-heading`}>
          <div className="specimen-section-heading">
            <div>
              <p className="eyebrow">Prepared revision 03</p>
              <h4 id={`${direction.id}-plot-heading`}>Light across one day</h4>
            </div>
            <span className="data-reduction">15-minute preview</span>
          </div>
          <MiniChart directionId={direction.id} />
        </section>

        <aside className="specimen-form" aria-label="Illustrative analysis settings">
          <label htmlFor={`${direction.id}-variable`}>Primary variable</label>
          <select id={`${direction.id}-variable`} defaultValue="medi">
            <option value="medi">Melanopic EDI</option>
            <option value="photopic">Photopic illuminance</option>
          </select>
          <label htmlFor={`${direction.id}-grouping`}>Grouping</label>
          <select id={`${direction.id}-grouping`} defaultValue="date">
            <option value="date">Participant + date</option>
            <option value="participant">Participant only</option>
          </select>
          <button className="specimen-button" type="button" onClick={() => setApplied(true)}>
            {applied ? "Preview applied" : "Apply preview"}
          </button>
          <p className="focus-note">Use Tab to inspect the visible focus treatment.</p>
        </aside>
      </div>

      <div className="specimen-table-wrap">
        <table className="specimen-table">
          <caption>Illustrative prepared-data summary</caption>
          <thead>
            <tr>
              <th scope="col">Participant</th>
              <th scope="col">Observed</th>
              <th scope="col">Missing</th>
              <th scope="col">Median</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <th scope="row">P01</th>
              <td>23 h 15 min</td>
              <td>3.1%</td>
              <td>118 lx</td>
            </tr>
            <tr>
              <th scope="row">P02</th>
              <td>21 h 40 min</td>
              <td>9.7%</td>
              <td>94 lx</td>
            </tr>
          </tbody>
        </table>
      </div>

      <div className="specimen-states" aria-label="System-state specimens">
        <StatusChip symbol="○" label="Idle" kind="idle" />
        <StatusChip symbol="↻" label="Running" kind="running" />
        <StatusChip symbol="▲" label="Warning" kind="warning" />
        <StatusChip symbol="✓" label="Complete" kind="complete" />
        <StatusChip symbol="×" label="Stale" kind="stale" />
      </div>
    </div>
  );
}

function Palette({ direction }) {
  return (
    <div className="palette" role="list" aria-label={`${direction.name} palette`}>
      {direction.palette.map((color) => (
        <div className="palette__item" role="listitem" key={color.role}>
          <span className="palette__swatch" style={{ backgroundColor: color.value }} aria-hidden="true" />
          <span>
            <strong>{color.role}</strong>
            <code>{color.value}</code>
          </span>
        </div>
      ))}
    </div>
  );
}

function DirectionSection({ direction }) {
  return (
    <article id={direction.id} className={`direction ${direction.className}`}>
      <header className="direction-header">
        <div>
          <p className="eyebrow">Direction {direction.number}</p>
          <div className="direction-title-line">
            <h2>{direction.name}</h2>
            {direction.recommended ? <span className="recommendation-label">Recommended</span> : null}
          </div>
          <p className="direction-promise">{direction.promise}</p>
          <p className="direction-description">{direction.description}</p>
        </div>
        <HexMark directionId={direction.id} />
      </header>

      <div className="keyword-row" aria-label={`${direction.name} keywords`}>
        {direction.keywords.map((keyword) => (
          <span key={keyword}>{keyword}</span>
        ))}
      </div>

      <figure className="atmosphere">
        <img src={direction.image} alt={direction.imageAlt} />
        <figcaption>Original generated atmosphere · non-shipping mood reference</figcaption>
      </figure>

      <section className="direction-system" aria-labelledby={`${direction.id}-system-heading`}>
        <h3 id={`${direction.id}-system-heading`}>System DNA</h3>
        <div className="system-grid">
          <div>
            <h4>Semantic palette</h4>
            <Palette direction={direction} />
          </div>
          <div className="type-specimen">
            <h4>Typography</h4>
            <p className="type-specimen__name">{direction.typography}</p>
            <p className="type-specimen__sample">Light decisions should remain visible.</p>
            <p>{direction.typeDetail}</p>
            <code>2026-07-17 · revision 03 · 15 min</code>
          </div>
          <div className="logo-territory">
            <h4>Logo territory</h4>
            <HexMark directionId={direction.id} />
            <p>Flat, editable SVG study. The hex establishes kinship; the internal time-and-light structure creates distinction.</p>
          </div>
        </div>
      </section>

      <section className="direction-components" aria-labelledby={`${direction.id}-components-heading`}>
        <div className="section-heading-row">
          <div>
            <p className="eyebrow">Inspectable specimen</p>
            <h3 id={`${direction.id}-components-heading`}>Components, data, and states</h3>
          </div>
          <p>Shared content and structure keep the four directions comparable. This is not final app navigation.</p>
        </div>
        <ComponentSpecimen direction={direction} />
      </section>

      <section className="direction-assessment" aria-label={`${direction.name} assessment`}>
        <div>
          <h3>Where it is strongest</h3>
          <ul>
            {direction.strengths.map((strength) => (
              <li key={strength}>{strength}</li>
            ))}
          </ul>
        </div>
        <div>
          <h3>Accessibility posture</h3>
          <p>{direction.accessibility}</p>
        </div>
        <div>
          <h3>Watch-out</h3>
          <p>{direction.tradeoff}</p>
        </div>
      </section>
    </article>
  );
}

function ComparisonTable() {
  const rows = [
    ["Primary feeling", "Calm scientific clarity", "Instrument precision", "Human explanation", "Minimal professionalism"],
    ["Scientific credibility", "Strong", "Strongest", "Strong", "Strong"],
    ["Beginner approachability", "Strong", "Mixed", "Strongest", "Strong"],
    ["LightLogR kinship", "Hex + circadian arc", "Hex + measured beam", "Hex + sun-path", "Hex + measure band"],
    ["Distinctiveness", "Strong", "Strongest", "Strong", "Mixed"],
    ["Data-visualization fit", "Strong", "Strongest", "Mixed", "Strong"],
    ["Accessibility", "Strong", "Strong", "Strong", "Strongest"],
    ["bslib feasibility", "Strong", "Strong", "Strong", "Strongest"],
    ["Information density", "Balanced", "Highest", "Moderate", "Balanced"],
    ["Main risk", "Could become generic", "Could feel intimidating", "Could feel too editorial", "Could feel interchangeable"]
  ];

  return (
    <div className="comparison-table-wrap">
      <table className="comparison-table">
        <caption>Consistent comparison of the four visual territories</caption>
        <thead>
          <tr>
            <th scope="col">Decision axis</th>
            <th scope="col">Circadian Field</th>
            <th scope="col">Spectral Console</th>
            <th scope="col">Daylight Almanac</th>
            <th scope="col">Clear Measure</th>
          </tr>
        </thead>
        <tbody>
          {rows.map(([axis, ...values]) => (
            <tr key={axis}>
              <th scope="row">{axis}</th>
              {values.map((value, index) => (
                <td key={`${axis}-${index}`}>{value}</td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function ReviewPanel() {
  const [review, setReview] = useState(createEmptyReview);
  const [activeDirection, setActiveDirection] = useState(directionIds[0]);
  const [hydrated, setHydrated] = useState(false);
  const [copyStatus, setCopyStatus] = useState("");

  useEffect(() => {
    try {
      const stored = window.localStorage.getItem(REVIEW_STORAGE_KEY);
      if (stored) {
        setReview(normalizeReview(JSON.parse(stored)));
      }
    } catch {
      setReview(createEmptyReview());
    } finally {
      setHydrated(true);
    }
  }, []);

  useEffect(() => {
    if (!hydrated) return;
    window.localStorage.setItem(REVIEW_STORAGE_KEY, JSON.stringify(review));
  }, [review, hydrated]);

  const summary = useMemo(() => buildReviewSummary(review), [review]);

  function updateRating(criterion, rating) {
    setReview((current) => ({
      ...current,
      ratings: {
        ...current.ratings,
        [activeDirection]: {
          ...current.ratings[activeDirection],
          [criterion]: rating
        }
      }
    }));
  }

  function updateNotes(notes) {
    setReview((current) => ({
      ...current,
      notes: { ...current.notes, [activeDirection]: notes }
    }));
  }

  async function copySummary() {
    try {
      await navigator.clipboard.writeText(summary);
      setCopyStatus("Review summary copied.");
    } catch {
      setCopyStatus("Copy unavailable here. Select the review summary below and copy it manually.");
    }
  }

  return (
    <section id="review" className="review-section" aria-labelledby="review-heading">
      <div className="section-heading-row">
        <div>
          <p className="eyebrow">Decision workspace</p>
          <h2 id="review-heading">Record your review</h2>
        </div>
        <p>Ratings and notes stay only in this browser. Nothing is submitted or stored on a server.</p>
      </div>

      <div className="review-tabs" role="group" aria-label="Direction to score">
        {directionIds.map((directionId) => (
          <button
            key={directionId}
            type="button"
            className={activeDirection === directionId ? "is-active" : ""}
            aria-pressed={activeDirection === directionId}
            onClick={() => setActiveDirection(directionId)}
          >
            {directionNames[directionId]}
          </button>
        ))}
      </div>

      <div className="review-form-panel">
        <h3>{directionNames[activeDirection]}</h3>
        <div className="rating-grid">
          {criteria.map((criterion, index) => {
            const inputId = `${activeDirection}-criterion-${index}`;
            return (
              <label key={criterion} htmlFor={inputId}>
                <span>{criterion}</span>
                <select
                  id={inputId}
                  value={review.ratings[activeDirection][criterion]}
                  onChange={(event) => updateRating(criterion, event.target.value)}
                >
                  <option value="">Not rated</option>
                  <option value="Strong">Strong</option>
                  <option value="Mixed">Mixed</option>
                  <option value="Weak">Weak</option>
                </select>
              </label>
            );
          })}
        </div>
        <label className="full-field" htmlFor={`${activeDirection}-notes`}>
          Notes on {directionNames[activeDirection]}
          <textarea
            id={`${activeDirection}-notes`}
            value={review.notes[activeDirection]}
            onChange={(event) => updateNotes(event.target.value)}
            rows="5"
            placeholder="What feels right, risky, or worth carrying forward?"
          />
        </label>
      </div>

      <fieldset className="preference-fieldset">
        <legend>Preferred direction</legend>
        <div className="preference-options">
          {directionIds.map((directionId) => (
            <label key={directionId}>
              <input
                type="radio"
                name="preferred-direction"
                value={directionId}
                checked={review.preferredDirection === directionId}
                onChange={(event) =>
                  setReview((current) => ({ ...current, preferredDirection: event.target.value }))
                }
              />
              <span>{directionNames[directionId]}</span>
            </label>
          ))}
        </div>
      </fieldset>

      <label className="full-field" htmlFor="borrowed-elements">
        Optional elements to borrow from another direction
        <textarea
          id="borrowed-elements"
          value={review.borrowedElements}
          onChange={(event) =>
            setReview((current) => ({ ...current, borrowedElements: event.target.value }))
          }
          rows="4"
          placeholder="For example: use Spectral Console’s table density within Circadian Field."
        />
      </label>

      <div className="summary-panel">
        <div className="summary-panel__heading">
          <div>
            <h3>Review summary</h3>
            <p>Paste this summary into the Codex task to create the selected-direction brief.</p>
          </div>
          <button type="button" className="primary-action" onClick={copySummary}>
            Copy review summary
          </button>
        </div>
        <label className="sr-only" htmlFor="review-summary">Generated review summary</label>
        <textarea id="review-summary" className="review-summary" value={summary} readOnly rows="18" />
        <p className="copy-status" role="status" aria-live="polite">{copyStatus}</p>
      </div>
    </section>
  );
}

export default function WorkshopPage() {
  const [theme, setTheme] = useState("light");

  useEffect(() => {
    const stored = window.localStorage.getItem(THEME_STORAGE_KEY);
    const initialTheme = stored === "dark" ? "dark" : "light";
    setTheme(initialTheme);
    document.documentElement.dataset.theme = initialTheme;
  }, []);

  function chooseTheme(nextTheme) {
    setTheme(nextTheme);
    document.documentElement.dataset.theme = nextTheme;
    window.localStorage.setItem(THEME_STORAGE_KEY, nextTheme);
  }

  return (
    <>
      <Head>
        <title>LightLogWeb Visual Direction Workshop</title>
        <meta
          name="description"
          content="Owner review of four accessible visual directions for LightLogWeb Milestone 2."
        />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
      </Head>

      <a className="skip-link" href="#main-content">Skip to workshop content</a>

      <header className="site-header">
        <div className="site-header__inner">
          <div className="site-identity">
            <HexMark directionId="circadian-field" size="small" />
            <span>LightLogWeb</span>
          </div>
          <nav aria-label="Workshop sections">
            <a href="#heritage">Heritage</a>
            <a href="#directions">Directions</a>
            <a href="#compare">Compare</a>
            <a href="#review">Review</a>
          </nav>
          <fieldset className="theme-control">
            <legend className="sr-only">Workshop color mode</legend>
            <button
              type="button"
              aria-pressed={theme === "light"}
              onClick={() => chooseTheme("light")}
            >
              Light
            </button>
            <button
              type="button"
              aria-pressed={theme === "dark"}
              onClick={() => chooseTheme("dark")}
            >
              Dark
            </button>
          </fieldset>
        </div>
      </header>

      <main id="main-content">
        <section className="hero" aria-labelledby="hero-heading">
          <div className="hero__content">
            <p className="eyebrow">Milestone 2 · Owner workshop</p>
            <h1 id="hero-heading">Make light exposure legible across time.</h1>
            <p className="hero__lede">
              Four visual directions test the same promise: reveal consequential choices, calm complexity,
              and communicate every state without relying on color alone.
            </p>
            <div className="hero__actions">
              <a className="primary-action" href="#directions">Review directions</a>
              <a className="secondary-action" href="#review">Open scorecard</a>
            </div>
            <ul className="hero__constraints" aria-label="Workshop constraints">
              <li>Family resemblance, not imitation</li>
              <li>Light and dark modes</li>
              <li>WCAG 2.2 AA target</li>
              <li>bslib-compatible</li>
            </ul>
          </div>
          <div className="hero__mark" aria-hidden="true">
            <HexMark directionId="circadian-field" />
            <span className="hero__orbit" />
          </div>
        </section>

        <section className="narrative" aria-labelledby="narrative-heading">
          <div className="section-intro">
            <p className="eyebrow">Shared design narrative</p>
            <h2 id="narrative-heading">Trust comes from visible structure.</h2>
            <p>
              LightLogWeb serves researchers moving between first inspection and reproducible analysis.
              The interface should feel calm enough to learn and exact enough to trust.
            </p>
          </div>
          <ol className="principle-list">
            <li>
              <span>01</span>
              <div><h3>Make time visible</h3><p>Use cycles, horizons, and temporal bands only when they clarify duration, sequence, or change.</p></div>
            </li>
            <li>
              <span>02</span>
              <div><h3>Reveal consequence</h3><p>Draft, preview, apply, reduction, grouping, and revision states remain explicit at the point of decision.</p></div>
            </li>
            <li>
              <span>03</span>
              <div><h3>Calm complexity</h3><p>Hierarchy and progressive disclosure do the work; ornamental glow and dashboard clutter do not.</p></div>
            </li>
            <li>
              <span>04</span>
              <div><h3>Communicate every state</h3><p>Words, symbols, focus, and layout reinforce color across empty, running, warning, error, complete, and stale states.</p></div>
            </li>
          </ol>
        </section>

        <section id="heritage" className="heritage" aria-labelledby="heritage-heading">
          <div className="heritage-reference">
            <img src="/reference-lightlogr-logo.png" alt="Current LightLogR three-dimensional hex logo" />
            <p>
              Heritage reference from the MIT-licensed <a href="https://tscnlab.github.io/LightLogR/" target="_blank" rel="noreferrer">LightLogR project</a>.
            </p>
          </div>
          <div>
            <p className="eyebrow">Family resemblance</p>
            <h2 id="heritage-heading">Keep the hex. Flatten the relationship.</h2>
            <div className="heritage-columns">
              <div>
                <h3>Carry forward</h3>
                <ul>
                  <li>A recognizable hexagonal silhouette</li>
                  <li>Directed light as a core metaphor</li>
                  <li>An explicit LightLogR ecosystem relationship</li>
                </ul>
              </div>
              <div>
                <h3>Leave behind</h3>
                <ul>
                  <li>Dark three-dimensional room geometry</li>
                  <li>Barcode spotlight and photographic effects</li>
                  <li>Perspective-dependent legibility</li>
                </ul>
              </div>
            </div>
            <p className="attribution-note">
              TUM, TSCN, MeLiDos, and funder marks remain attribution identities in About and funding contexts—not ingredients of the product mark.
            </p>
          </div>
        </section>

        <section id="directions" className="directions-intro" aria-labelledby="directions-heading">
          <p className="eyebrow">Four coherent territories</p>
          <h2 id="directions-heading">Same evidence. Different character.</h2>
          <p>
            Each direction uses equivalent mood, type, palette, mark, component, plot, table, form, focus, and state specimens so the comparison stays honest.
          </p>
        </section>

        <div className="direction-stack">
          {directions.map((direction) => (
            <DirectionSection direction={direction} key={direction.id} />
          ))}
        </div>

        <section id="compare" className="comparison-section" aria-labelledby="comparison-heading">
          <div className="section-heading-row">
            <div>
              <p className="eyebrow">Decision matrix</p>
              <h2 id="comparison-heading">Compare the trade-offs directly</h2>
            </div>
            <p>No direction wins every axis. The choice is which trade-off best serves the product.</p>
          </div>
          <ComparisonTable />
        </section>

        <section id="recommendation" className="recommendation" aria-labelledby="recommendation-heading">
          <div className="recommendation__mark">
            <HexMark directionId="circadian-field" />
          </div>
          <div>
            <p className="eyebrow">Recommended starting point</p>
            <h2 id="recommendation-heading">Choose Circadian Field as the base system.</h2>
            <p className="recommendation__lead">
              It best balances scientific credibility, beginner invitation, plot neutrality, accessible state design, and direct bslib implementation.
            </p>
            <ul>
              <li>Use Spectral Console’s alignment discipline for dense tables, revisions, and diagnostics.</li>
              <li>Use Daylight Almanac’s patient explanatory voice for onboarding, empty states, and decision summaries.</li>
              <li>Use Clear Measure’s reduction discipline when an element does not improve comprehension or trust.</li>
              <li>Do not import any alternative’s full visual character; the selected system should remain coherent.</li>
            </ul>
            <div className="recommendation-risks">
              <p><strong>Spectral Console risk:</strong> precision may read as difficulty before the user begins.</p>
              <p><strong>Daylight Almanac risk:</strong> warmth may compete with dense analysis and scientific neutrality.</p>
              <p><strong>Clear Measure risk:</strong> professional neutrality may feel interchangeable without distinctive temporal cues.</p>
            </div>
          </div>
        </section>

        <ReviewPanel />
      </main>

      <footer>
        <p>LightLogWeb Milestone 2 visual-direction workshop · Institutional and funder identities remain attribution-only.</p>
        <a href="#main-content">Back to top</a>
      </footer>
    </>
  );
}
