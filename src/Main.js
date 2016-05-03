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

var empty = {};

exports.empty = empty;

exports.insert = function (klass) {
  return function (val) {
    return function (set) {
      var key = klass.crappyHash(val);
      if (!set[key]) {
        if (set === empty) {
          set = {};
        }
        set[key] = val;
      }
      return set;
    }
  }
}

exports.mapToArray = function (mapper) {
  return function (set) {
    var array = [];
    for (var key in set) {
      var val = set[key];
      array.push(mapper(val));
    }
    return array;
  }
}
