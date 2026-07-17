const pairs = [
  ["Workshop light text", "#182129", "#f3f5f2", 7],
  ["Workshop light muted", "#4f5d65", "#f3f5f2", 4.5],
  ["Workshop light control boundary", "#687772", "#ffffff", 3],
  ["Workshop light focus ring", "#006b79", "#f3f5f2", 3],
  ["Workshop dark text", "#f1f5f3", "#11191d", 7],
  ["Workshop dark muted", "#bdc9c7", "#11191d", 4.5],
  ["Workshop dark control boundary", "#7e9298", "#18242a", 3],
  ["Workshop dark focus ring", "#8fe6ef", "#11191d", 3],
  ["Circadian light text", "#102a33", "#f6f8f4", 7],
  ["Circadian light muted", "#52666d", "#f6f8f4", 4.5],
  ["Circadian primary button", "#ffffff", "#0b6675", 4.5],
  ["Circadian light control boundary", "#60767b", "#ffffff", 3],
  ["Circadian light focus ring", "#006d7a", "#f6f8f4", 3],
  ["Circadian light chart line", "#0b6675", "#ffffff", 3],
  ["Circadian light chart point", "#c36a1d", "#ffffff", 3],
  ["Circadian light idle state", "#52666d", "#f6f8f4", 4.5],
  ["Circadian light running state", "#0b6675", "#f6f8f4", 4.5],
  ["Circadian light warning state", "#8a4b09", "#f6f8f4", 4.5],
  ["Circadian light complete state", "#1e6f4a", "#f6f8f4", 4.5],
  ["Circadian light stale state", "#a33b3b", "#f6f8f4", 4.5],
  ["Circadian dark text", "#eef7f5", "#081a20", 7],
  ["Circadian dark muted", "#b7c7c5", "#081a20", 4.5],
  ["Circadian dark button", "#07171c", "#76d2dd", 4.5],
  ["Circadian dark control boundary", "#74939a", "#10282f", 3],
  ["Circadian dark focus ring", "#9cebf2", "#081a20", 3],
  ["Circadian dark chart line", "#76d2dd", "#10282f", 3],
  ["Circadian dark chart point", "#f2b66d", "#10282f", 3],
  ["Circadian dark idle state", "#b7c7c5", "#081a20", 4.5],
  ["Circadian dark running state", "#76d2dd", "#081a20", 4.5],
  ["Circadian dark warning state", "#ffc47d", "#081a20", 4.5],
  ["Circadian dark complete state", "#83d5aa", "#081a20", 4.5],
  ["Circadian dark stale state", "#ffaaaa", "#081a20", 4.5],
  ["Spectral light text", "#111827", "#f4f6f8", 7],
  ["Spectral light muted", "#536175", "#f4f6f8", 4.5],
  ["Spectral primary button", "#ffffff", "#174ea6", 4.5],
  ["Spectral light control boundary", "#66758a", "#ffffff", 3],
  ["Spectral light focus ring", "#1c5dc4", "#f4f6f8", 3],
  ["Spectral light chart line", "#174ea6", "#ffffff", 3],
  ["Spectral light chart point", "#7048d8", "#ffffff", 3],
  ["Spectral light idle state", "#536175", "#f4f6f8", 4.5],
  ["Spectral light running state", "#174ea6", "#f4f6f8", 4.5],
  ["Spectral light warning state", "#805000", "#f4f6f8", 4.5],
  ["Spectral light complete state", "#1e6b50", "#f4f6f8", 4.5],
  ["Spectral light stale state", "#a33a4d", "#f4f6f8", 4.5],
  ["Spectral dark text", "#f0f4fa", "#0f1218", 7],
  ["Spectral dark muted", "#b8c3d1", "#0f1218", 4.5],
  ["Spectral dark button", "#07111f", "#8bb6ff", 4.5],
  ["Spectral dark control boundary", "#74859a", "#181e28", 3],
  ["Spectral dark focus ring", "#a9c9ff", "#0f1218", 3],
  ["Spectral dark chart line", "#8bb6ff", "#181e28", 3],
  ["Spectral dark chart point", "#b69cff", "#181e28", 3],
  ["Spectral dark idle state", "#b8c3d1", "#0f1218", 4.5],
  ["Spectral dark running state", "#8bb6ff", "#0f1218", 4.5],
  ["Spectral dark warning state", "#ffd181", "#0f1218", 4.5],
  ["Spectral dark complete state", "#8bd4b3", "#0f1218", 4.5],
  ["Spectral dark stale state", "#ffadb9", "#0f1218", 4.5],
  ["Almanac light text", "#2a2637", "#fbf7ef", 7],
  ["Almanac light muted", "#655e70", "#fbf7ef", 4.5],
  ["Almanac primary button", "#ffffff", "#455491", 4.5],
  ["Almanac light control boundary", "#786f64", "#fffdf8", 3],
  ["Almanac light focus ring", "#344583", "#fbf7ef", 3],
  ["Almanac light chart line", "#455491", "#fffdf8", 3],
  ["Almanac light chart point", "#a74736", "#fffdf8", 3],
  ["Almanac light idle state", "#655e70", "#fbf7ef", 4.5],
  ["Almanac light running state", "#455491", "#fbf7ef", 4.5],
  ["Almanac light warning state", "#815000", "#fbf7ef", 4.5],
  ["Almanac light complete state", "#356a4c", "#fbf7ef", 4.5],
  ["Almanac light stale state", "#9b3d3d", "#fbf7ef", 4.5],
  ["Almanac dark text", "#fff7ed", "#1d1a24", 7],
  ["Almanac dark muted", "#d0c7bc", "#1d1a24", 4.5],
  ["Almanac dark button", "#111425", "#aeb9ff", 4.5],
  ["Almanac dark control boundary", "#8b8292", "#292532", 3],
  ["Almanac dark focus ring", "#c2cbff", "#1d1a24", 3],
  ["Almanac dark chart line", "#aeb9ff", "#292532", 3],
  ["Almanac dark chart point", "#ff9b83", "#292532", 3],
  ["Almanac dark idle state", "#d0c7bc", "#1d1a24", 4.5],
  ["Almanac dark running state", "#aeb9ff", "#1d1a24", 4.5],
  ["Almanac dark warning state", "#ffd08a", "#1d1a24", 4.5],
  ["Almanac dark complete state", "#9bd6ae", "#1d1a24", 4.5],
  ["Almanac dark stale state", "#ffb0a7", "#1d1a24", 4.5],
  ["Clear light text", "#17232b", "#f8fafb", 7],
  ["Clear light muted", "#56656e", "#f8fafb", 4.5],
  ["Clear primary button", "#ffffff", "#255b8f", 4.5],
  ["Clear light control boundary", "#61717b", "#ffffff", 3],
  ["Clear light focus ring", "#1d5f9b", "#f8fafb", 3],
  ["Clear light chart line", "#255b8f", "#ffffff", 3],
  ["Clear light chart point", "#4c738f", "#ffffff", 3],
  ["Clear light idle state", "#56656e", "#f8fafb", 4.5],
  ["Clear light running state", "#255b8f", "#f8fafb", 4.5],
  ["Clear light warning state", "#7c4d00", "#f8fafb", 4.5],
  ["Clear light complete state", "#276749", "#f8fafb", 4.5],
  ["Clear light stale state", "#9c3d3d", "#f8fafb", 4.5],
  ["Clear dark text", "#f2f6f8", "#10171c", 7],
  ["Clear dark muted", "#bac6cc", "#10171c", 4.5],
  ["Clear dark button", "#08131d", "#8cc1f0", 4.5],
  ["Clear dark control boundary", "#7b8e98", "#19232a", 3],
  ["Clear dark focus ring", "#a8d4f7", "#10171c", 3],
  ["Clear dark chart line", "#8cc1f0", "#19232a", 3],
  ["Clear dark chart point", "#b4cee5", "#19232a", 3],
  ["Clear dark idle state", "#bac6cc", "#10171c", 4.5],
  ["Clear dark running state", "#8cc1f0", "#10171c", 4.5],
  ["Clear dark warning state", "#ffd08a", "#10171c", 4.5],
  ["Clear dark complete state", "#8ed2ad", "#10171c", 4.5],
  ["Clear dark stale state", "#ffadb0", "#10171c", 4.5]
];

function channel(value) {
  const normalized = value / 255;
  return normalized <= 0.04045
    ? normalized / 12.92
    : ((normalized + 0.055) / 1.055) ** 2.4;
}

function luminance(hex) {
  const value = hex.replace("#", "");
  const channels = [0, 2, 4].map((offset) => Number.parseInt(value.slice(offset, offset + 2), 16));
  return 0.2126 * channel(channels[0]) + 0.7152 * channel(channels[1]) + 0.0722 * channel(channels[2]);
}

function ratio(foreground, background) {
  const values = [luminance(foreground), luminance(background)].sort((a, b) => b - a);
  return (values[0] + 0.05) / (values[1] + 0.05);
}

let failed = false;
for (const [label, foreground, background, minimum] of pairs) {
  const measured = ratio(foreground, background);
  const passed = measured >= minimum;
  console.log(`${passed ? "PASS" : "FAIL"} ${label}: ${measured.toFixed(2)}:1 (minimum ${minimum}:1)`);
  failed ||= !passed;
}

if (failed) {
  process.exitCode = 1;
}
