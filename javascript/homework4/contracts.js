'use strict';

function withExpected(predicate, expectedDescription) {
  predicate.expected = expectedDescription;
  return predicate;
}

function expect(predicate) {
  if (predicate && predicate.expected) {
    return predicate.expected;
  }
  if (predicate && predicate.name) {
    return predicate.name;
  }
  return 'ANONYMOUS CONTRACT';
}

function any(_value) {
  return true;
}

const isNumber = withExpected(function isNumber(value) {
  return typeof value === 'number' && !Number.isNaN(value);
}, 'number');

const isBoolean = withExpected(function isBoolean(value) {
  return typeof value === 'boolean';
}, 'boolean');

const isDefined = withExpected(function isDefined(value) {
  return value !== null && typeof value !== 'undefined';
}, 'defined');

const isString = withExpected(function isString(value) {
  return typeof value === 'string' || value instanceof String;
}, 'string');

const isNegative = withExpected(function isNegative(value) {
  return isNumber(value) && value < 0;
}, 'negative number');

const isPositive = withExpected(function isPositive(value) {
  return isNumber(value) && value > 0;
}, 'positive number');

function and() {
  const predicates = Array.prototype.slice.call(arguments);
  const combined = function(v) {
    for (let i = 0; i < predicates.length; i++) {
      const predicate = predicates[i];
      if (typeof predicate !== 'function' || !predicate.call(this, v)) {
        return false;
      }
    }
    return true;
  };

  combined.expected = expect(predicates[0]);
  for (let i = 1; i < predicates.length; i++) {
    combined.expected += ` and ${expect(predicates[i])}`;
  }
  return combined;
}

function or() {
  const predicates = Array.prototype.slice.call(arguments);
  const combined = function(v) {
    for (let i = 0; i < predicates.length; i++) {
      const predicate = predicates[i];
      if (typeof predicate === 'function' && predicate.call(this, v)) {
        return true;
      }
    }
    return false;
  };

  combined.expected = expect(predicates[0]);
  for (let i = 1; i < predicates.length; i++) {
    combined.expected += ` or ${expect(predicates[i])}`;
  }
  return combined;
}

function not(predicate) {
  const negated = function(v) {
    return !predicate.call(this, v);
  };
  negated.expected = `not ${expect(predicate)}`;
  return negated;
}

function contract(preconditions, postcondition, targetFunction) {
  return new Proxy(targetFunction, {
    apply(target, thisArg, argumentsList) {
      for (let i = 0; i < preconditions.length; i++) {
        const predicate = preconditions[i];
        const value = argumentsList[i];
        if (!predicate.call(thisArg, value)) {
          const message = `Contract violation in position ${i}. Expected ${expect(predicate)} but received ${value}.  Blame -> Top-level code`;
          const err = new Error(message);
          err.blame = 'caller';
          throw err;
        }
      }

      const returnValue = Reflect.apply(target, thisArg, argumentsList);
      if (!postcondition.call(thisArg, returnValue)) {
        const blame = target.name || 'ANONYMOUS FUNCTION';
        const message = `Contract violation. Expected ${expect(postcondition)} but returned ${returnValue}. Blame -> ${blame}`;
        const err = new Error(message);
        err.blame = 'library';
        throw err;
      }

      return returnValue;
    }
  });
}

module.exports = {
  any,
  isNumber,
  isBoolean,
  isDefined,
  isString,
  isNegative,
  isPositive,
  isInteger: Number.isInteger,
  and,
  not,
  or,
  contract,
  withExpected,
  negative: isNegative
};
