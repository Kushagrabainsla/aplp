'use strict';

const assert = require('assert');
const {
  any,
  isNumber,
  isBoolean,
  isDefined,
  isString,
  isNegative,
  isPositive,
  and,
  not,
  or,
  contract,
  withExpected
} = require('./contracts');

describe('basic contracts', () => {
  it('any accepts all values', () => {
    assert.strictEqual(any(42), true);
    assert.strictEqual(any(null), true);
    assert.strictEqual(any(undefined), true);
  });

  it('isNumber works as expected', () => {
    assert.strictEqual(isNumber(1), true);
    assert.strictEqual(isNumber(0), true);
    assert.strictEqual(isNumber(NaN), false);
    assert.strictEqual(isNumber('1'), false);
  });

  it('isBoolean works as expected', () => {
    assert.strictEqual(isBoolean(true), true);
    assert.strictEqual(isBoolean(false), true);
    assert.strictEqual(isBoolean(1), false);
  });

  it('isDefined rejects null and undefined', () => {
    assert.strictEqual(isDefined('x'), true);
    assert.strictEqual(isDefined(0), true);
    assert.strictEqual(isDefined(null), false);
    assert.strictEqual(isDefined(undefined), false);
  });

  it('isString works as expected', () => {
    assert.strictEqual(isString('hello'), true);
    assert.strictEqual(isString(''), true);
    assert.strictEqual(isString(123), false);
  });

  it('isNegative and isPositive work as expected', () => {
    assert.strictEqual(isNegative(-1), true);
    assert.strictEqual(isNegative(0), false);
    assert.strictEqual(isPositive(1), true);
    assert.strictEqual(isPositive(0), false);
  });
});

describe('contract combinators', () => {
  it('and requires all predicates to pass', () => {
    const isPositiveNumber = and(isNumber, isPositive);
    assert.strictEqual(isPositiveNumber(5), true);
    assert.strictEqual(isPositiveNumber(-5), false);
    assert.strictEqual(isPositiveNumber('5'), false);
  });

  it('not negates a predicate', () => {
    const isNotNumber = not(isNumber);
    assert.strictEqual(isNotNumber('x'), true);
    assert.strictEqual(isNotNumber(10), false);
  });

  it('or requires at least one predicate to pass', () => {
    const isNumberOrString = or(isNumber, isString);
    assert.strictEqual(isNumberOrString('x'), true);
    assert.strictEqual(isNumberOrString(10), true);
    assert.strictEqual(isNumberOrString(false), false);
  });
});

describe('contract proxy', () => {
  it('allows valid input and valid output', () => {
    const add = contract([isNumber, isNumber], isNumber, function add(a, b) {
      return a + b;
    });

    assert.strictEqual(add(2, 3), 5);
  });

  it('blames caller when a precondition fails', () => {
    const add = contract([isNumber, isNumber], isNumber, function add(a, b) {
      return a + b;
    });

    assert.throws(
      () => add('2', 3),
      (error) => error.blame === 'caller' && /position 0/.test(error.message)
    );
  });

  it('blames library when the postcondition fails', () => {
    const badAdd = contract([isNumber, isNumber], isNumber, function badAdd(a, b) {
      return String(a + b);
    });

    assert.throws(
      () => badAdd(2, 3),
      (error) => error.blame === 'library' && /returned/.test(error.message)
    );
  });

  it('supports this-based predicates for object methods', () => {
    const enoughBalance = withExpected(function enoughBalance(withdrawalAmount) {
      return this.balance >= withdrawalAmount;
    }, 'balance >= withdrawal amount');

    const account = {
      balance: 50,
      withdraw: contract([enoughBalance, isPositive], isNumber, function withdraw(amount, fee) {
        this.balance -= amount + fee;
        return this.balance;
      })
    };

    assert.strictEqual(account.withdraw(10, 1), 39);
    assert.throws(
      () => account.withdraw(100, 1),
      (error) => error.blame === 'caller' && /position 0/.test(error.message)
    );
  });

});
