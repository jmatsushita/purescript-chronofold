const core = require("../output/Data.Chronofold.Core")
const buffer = require("../output/Data.Chronofold.Buffer")
const typs = require("../output/Data.Chronofold.Types")
module.exports.core = core
module.exports.buffer = buffer
module.exports.types = typs

// Example usage from JS

const {root, appendOp, appendOps, emptyLog, buildSnocStringOps} = core
const {project} = buffer;
const {Replica, showLog} = typs;


const alpha = Replica(1)
// console.log("alpha", alpha)
const alphaLogRoot = appendOp(emptyLog(alpha))(root(alpha));
// logShow(alphaLogRoot)();
console.log("alphaLogRoot", project(alphaLogRoot))

const callback = (event) => {
  console.log("event", event);
  const arrText = event.contentChanges.map(({text}) => text)

  let logAlpha = alphaLogRoot;
  arrText.forEach((str) => {
    console.log("str", str)
    const ops = buildSnocStringOps(logAlpha)(str)
    console.log("ops", ops)
    logAlpha = appendOps(logAlpha)(ops)
  })
  console.log("logAlpha", logAlpha)
  console.log(project(logAlpha))
}

callback({contentChanges: [{text: "a"}]})