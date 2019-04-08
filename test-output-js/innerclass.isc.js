if (typeof kotlin === 'undefined') {
  throw new Error("Error loading module 'innerclass.isc'. Its dependency 'kotlin' was not found. Please, check whether 'kotlin' is loaded prior to 'innerclass.isc'.");
}
this['innerclass.isc'] = function (_, Kotlin) {
  'use strict';
  Kotlin.defineModule('innerclass.isc', _);
  return _;
}(typeof this['innerclass.isc'] === 'undefined' ? {} : this['innerclass.isc'], kotlin);
