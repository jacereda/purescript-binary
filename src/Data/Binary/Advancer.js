"use strict";

// module Data.Binary.Advancer

exports.advance = function(s) {
  return function(d) {
    return function() {
      var off = d.off;
      d.off += s;
      return off;
    };
  };
}
