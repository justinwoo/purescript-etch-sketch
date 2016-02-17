// module Main

exports.keydownP = function (constant) {
  var out = constant(0);

  window.addEventListener('keydown', function (e) {
    out.set(e.keyCode);
  });

  return function () {
    return out;
  }
}
