const fs = require("node:fs");
const sharp = require("sharp");

const [input, output, rawSize] = process.argv.slice(2);
const size = Number(rawSize);

if (!input || !output || !Number.isInteger(size) || size <= 0) {
  throw new Error("Usage: node render-brand-png.cjs INPUT.svg OUTPUT.png SIZE");
}

sharp(input, { density: 288 })
  .resize(size, size, { fit: "fill" })
  .png({ compressionLevel: 9, adaptiveFiltering: true })
  .toFile(output)
  .then((info) => {
    if (info.width !== size || info.height !== size || !fs.existsSync(output)) {
      throw new Error(`Unexpected output dimensions for ${output}`);
    }
  })
  .catch((error) => {
    process.stderr.write(`${error.stack || error.message}\n`);
    process.exitCode = 1;
  });
