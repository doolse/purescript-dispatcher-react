
exports.mapRef = function (f,dft,refs,s) {
  if (!refs[s]) return dft;
  return f(refs[s]);
}
