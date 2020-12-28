module.exports = function(config) {
  config.set({
    frameworks: ['mocha'],
    files: [
      "output/test.js",
    ],
    reporters: ['progress'],
    browsers: ['Firefox'],
    singleRun: true
  });
};