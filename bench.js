var process = require('process');
var {PerformanceObserver, performance } = require('perf_hooks');
var bench = require('./output/Benchmark');

const obs = new PerformanceObserver((items) => {
  var item = items.getEntries()[0]; 
  console.log(item.entryType + " / " + item.name, item.duration + "ms");
  // performance.clearMarks();
});
obs.observe({ entryTypes: ['measure'] });

performance.mark('main');
var res2 = bench.main()
performance.measure('main', 'main');
// console.log('res main', res2)

const mark2 = 'mark2'
performance.mark(mark2);
var arr = bench.arrayWhatever;
performance.measure('arrayWhatever', mark2);
// console.log('arr', arr.length)

performance.mark('mark3');
var arr = bench.foldWat;
performance.measure('foldWat', 'mark3');

performance.mark('buildBench');
var res = bench.buildBench(bench.arrayOps)
performance.measure('buildBench', 'buildBench');
// console.log('res buildOps', res)

performance.mark('appendStringBench');
var log = bench.appendStringBench(bench.arrayOps)
performance.measure('appendStringBench', 'appendStringBench');

performance.mark('projectBench');
var log = bench.projectBench(log)
performance.measure('projectBench', 'projectBench');

// Baseline
// measure / main 9.1396ms
// measure / arrayWhatever 0.0544ms
// measure / foldWat 0.0548ms
// measure / buildOps 3.6387ms ~ 20ms
// measure / appendStringBench 162.6363ms
// measure / projectBench 9.9092ms

// const used = process.memoryUsage();
// for (let key in used) {
//   console.log(`${key} ${Math.round(used[key] / 1024 / 1024 * 100) / 100} MB`);
// }
