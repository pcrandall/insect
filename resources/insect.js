(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.Insect = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
/*! decimal.js v7.5.1 https://github.com/MikeMcl/decimal.js/LICENCE */
;(function (globalScope) {
  'use strict';


  /*
   *  decimal.js v7.5.1
   *  An arbitrary-precision Decimal type for JavaScript.
   *  https://github.com/MikeMcl/decimal.js
   *  Copyright (c) 2017 Michael Mclaughlin <M8ch88l@gmail.com>
   *  MIT Licence
   */


  // -----------------------------------  EDITABLE DEFAULTS  ------------------------------------ //


    // The maximum exponent magnitude.
    // The limit on the value of `toExpNeg`, `toExpPos`, `minE` and `maxE`.
  var EXP_LIMIT = 9e15,                      // 0 to 9e15

    // The limit on the value of `precision`, and on the value of the first argument to
    // `toDecimalPlaces`, `toExponential`, `toFixed`, `toPrecision` and `toSignificantDigits`.
    MAX_DIGITS = 1e9,                        // 0 to 1e9

    // Base conversion alphabet.
    NUMERALS = '0123456789abcdef',

    // The natural logarithm of 10 (1025 digits).
    LN10 = '2.3025850929940456840179914546843642076011014886287729760333279009675726096773524802359972050895982983419677840422862486334095254650828067566662873690987816894829072083255546808437998948262331985283935053089653777326288461633662222876982198867465436674744042432743651550489343149393914796194044002221051017141748003688084012647080685567743216228355220114804663715659121373450747856947683463616792101806445070648000277502684916746550586856935673420670581136429224554405758925724208241314695689016758940256776311356919292033376587141660230105703089634572075440370847469940168269282808481184289314848524948644871927809676271275775397027668605952496716674183485704422507197965004714951050492214776567636938662976979522110718264549734772662425709429322582798502585509785265383207606726317164309505995087807523710333101197857547331541421808427543863591778117054309827482385045648019095610299291824318237525357709750539565187697510374970888692180205189339507238539205144634197265287286965110862571492198849978748873771345686209167058',

    // Pi (1025 digits).
    PI = '3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989380952572010654858632789',


    // The initial configuration properties of the Decimal constructor.
    Decimal = {

      // These values must be integers within the stated ranges (inclusive).
      // Most of these values can be changed at run-time using the `Decimal.config` method.

      // The maximum number of significant digits of the result of a calculation or base conversion.
      // E.g. `Decimal.config({ precision: 20 });`
      precision: 20,                         // 1 to MAX_DIGITS

      // The rounding mode used when rounding to `precision`.
      //
      // ROUND_UP         0 Away from zero.
      // ROUND_DOWN       1 Towards zero.
      // ROUND_CEIL       2 Towards +Infinity.
      // ROUND_FLOOR      3 Towards -Infinity.
      // ROUND_HALF_UP    4 Towards nearest neighbour. If equidistant, up.
      // ROUND_HALF_DOWN  5 Towards nearest neighbour. If equidistant, down.
      // ROUND_HALF_EVEN  6 Towards nearest neighbour. If equidistant, towards even neighbour.
      // ROUND_HALF_CEIL  7 Towards nearest neighbour. If equidistant, towards +Infinity.
      // ROUND_HALF_FLOOR 8 Towards nearest neighbour. If equidistant, towards -Infinity.
      //
      // E.g.
      // `Decimal.rounding = 4;`
      // `Decimal.rounding = Decimal.ROUND_HALF_UP;`
      rounding: 4,                           // 0 to 8

      // The modulo mode used when calculating the modulus: a mod n.
      // The quotient (q = a / n) is calculated according to the corresponding rounding mode.
      // The remainder (r) is calculated as: r = a - n * q.
      //
      // UP         0 The remainder is positive if the dividend is negative, else is negative.
      // DOWN       1 The remainder has the same sign as the dividend (JavaScript %).
      // FLOOR      3 The remainder has the same sign as the divisor (Python %).
      // HALF_EVEN  6 The IEEE 754 remainder function.
      // EUCLID     9 Euclidian division. q = sign(n) * floor(a / abs(n)). Always positive.
      //
      // Truncated division (1), floored division (3), the IEEE 754 remainder (6), and Euclidian
      // division (9) are commonly used for the modulus operation. The other rounding modes can also
      // be used, but they may not give useful results.
      modulo: 1,                             // 0 to 9

      // The exponent value at and beneath which `toString` returns exponential notation.
      // JavaScript numbers: -7
      toExpNeg: -7,                          // 0 to -EXP_LIMIT

      // The exponent value at and above which `toString` returns exponential notation.
      // JavaScript numbers: 21
      toExpPos:  21,                         // 0 to EXP_LIMIT

      // The minimum exponent value, beneath which underflow to zero occurs.
      // JavaScript numbers: -324  (5e-324)
      minE: -EXP_LIMIT,                      // -1 to -EXP_LIMIT

      // The maximum exponent value, above which overflow to Infinity occurs.
      // JavaScript numbers: 308  (1.7976931348623157e+308)
      maxE: EXP_LIMIT,                       // 1 to EXP_LIMIT

      // Whether to use cryptographically-secure random number generation, if available.
      crypto: false                          // true/false
    },


  // ----------------------------------- END OF EDITABLE DEFAULTS ------------------------------- //


    inexact, noConflict, quadrant,
    external = true,

    decimalError = '[DecimalError] ',
    invalidArgument = decimalError + 'Invalid argument: ',
    precisionLimitExceeded = decimalError + 'Precision limit exceeded',
    cryptoUnavailable = decimalError + 'crypto unavailable',

    mathfloor = Math.floor,
    mathpow = Math.pow,

    isBinary = /^0b([01]+(\.[01]*)?|\.[01]+)(p[+-]?\d+)?$/i,
    isHex = /^0x([0-9a-f]+(\.[0-9a-f]*)?|\.[0-9a-f]+)(p[+-]?\d+)?$/i,
    isOctal = /^0o([0-7]+(\.[0-7]*)?|\.[0-7]+)(p[+-]?\d+)?$/i,
    isDecimal = /^(\d+(\.\d*)?|\.\d+)(e[+-]?\d+)?$/i,

    BASE = 1e7,
    LOG_BASE = 7,
    MAX_SAFE_INTEGER = 9007199254740991,

    LN10_PRECISION = LN10.length - 1,
    PI_PRECISION = PI.length - 1,

    // Decimal.prototype object
    P = {};


  // Decimal prototype methods


  /*
   *  absoluteValue             abs
   *  ceil
   *  comparedTo                cmp
   *  cosine                    cos
   *  cubeRoot                  cbrt
   *  decimalPlaces             dp
   *  dividedBy                 div
   *  dividedToIntegerBy        divToInt
   *  equals                    eq
   *  floor
   *  greaterThan               gt
   *  greaterThanOrEqualTo      gte
   *  hyperbolicCosine          cosh
   *  hyperbolicSine            sinh
   *  hyperbolicTangent         tanh
   *  inverseCosine             acos
   *  inverseHyperbolicCosine   acosh
   *  inverseHyperbolicSine     asinh
   *  inverseHyperbolicTangent  atanh
   *  inverseSine               asin
   *  inverseTangent            atan
   *  isFinite
   *  isInteger                 isInt
   *  isNaN
   *  isNegative                isNeg
   *  isPositive                isPos
   *  isZero
   *  lessThan                  lt
   *  lessThanOrEqualTo         lte
   *  logarithm                 log
   *  [maximum]                 [max]
   *  [minimum]                 [min]
   *  minus                     sub
   *  modulo                    mod
   *  naturalExponential        exp
   *  naturalLogarithm          ln
   *  negated                   neg
   *  plus                      add
   *  precision                 sd
   *  round
   *  sine                      sin
   *  squareRoot                sqrt
   *  tangent                   tan
   *  times                     mul
   *  toBinary
   *  toDecimalPlaces           toDP
   *  toExponential
   *  toFixed
   *  toFraction
   *  toHexadecimal             toHex
   *  toNearest
   *  toNumber
   *  toOctal
   *  toPower                   pow
   *  toPrecision
   *  toSignificantDigits       toSD
   *  toString
   *  truncated                 trunc
   *  valueOf                   toJSON
   */


  /*
   * Return a new Decimal whose value is the absolute value of this Decimal.
   *
   */
  P.absoluteValue = P.abs = function () {
    var x = new this.constructor(this);
    if (x.s < 0) x.s = 1;
    return finalise(x);
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a whole number in the
   * direction of positive Infinity.
   *
   */
  P.ceil = function () {
    return finalise(new this.constructor(this), this.e + 1, 2);
  };


  /*
   * Return
   *   1    if the value of this Decimal is greater than the value of `y`,
   *  -1    if the value of this Decimal is less than the value of `y`,
   *   0    if they have the same value,
   *   NaN  if the value of either Decimal is NaN.
   *
   */
  P.comparedTo = P.cmp = function (y) {
    var i, j, xdL, ydL,
      x = this,
      xd = x.d,
      yd = (y = new x.constructor(y)).d,
      xs = x.s,
      ys = y.s;

    // Either NaN or ±Infinity?
    if (!xd || !yd) {
      return !xs || !ys ? NaN : xs !== ys ? xs : xd === yd ? 0 : !xd ^ xs < 0 ? 1 : -1;
    }

    // Either zero?
    if (!xd[0] || !yd[0]) return xd[0] ? xs : yd[0] ? -ys : 0;

    // Signs differ?
    if (xs !== ys) return xs;

    // Compare exponents.
    if (x.e !== y.e) return x.e > y.e ^ xs < 0 ? 1 : -1;

    xdL = xd.length;
    ydL = yd.length;

    // Compare digit by digit.
    for (i = 0, j = xdL < ydL ? xdL : ydL; i < j; ++i) {
      if (xd[i] !== yd[i]) return xd[i] > yd[i] ^ xs < 0 ? 1 : -1;
    }

    // Compare lengths.
    return xdL === ydL ? 0 : xdL > ydL ^ xs < 0 ? 1 : -1;
  };


  /*
   * Return a new Decimal whose value is the cosine of the value in radians of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-1, 1]
   *
   * cos(0)         = 1
   * cos(-0)        = 1
   * cos(Infinity)  = NaN
   * cos(-Infinity) = NaN
   * cos(NaN)       = NaN
   *
   */
  P.cosine = P.cos = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.d) return new Ctor(NaN);

    // cos(0) = cos(-0) = 1
    if (!x.d[0]) return new Ctor(1);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(x.e, x.sd()) + LOG_BASE;
    Ctor.rounding = 1;

    x = cosine(Ctor, toLessThanHalfPi(Ctor, x));

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return finalise(quadrant == 2 || quadrant == 3 ? x.neg() : x, pr, rm, true);
  };


  /*
   *
   * Return a new Decimal whose value is the cube root of the value of this Decimal, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   *  cbrt(0)  =  0
   *  cbrt(-0) = -0
   *  cbrt(1)  =  1
   *  cbrt(-1) = -1
   *  cbrt(N)  =  N
   *  cbrt(-I) = -I
   *  cbrt(I)  =  I
   *
   * Math.cbrt(x) = (x < 0 ? -Math.pow(-x, 1/3) : Math.pow(x, 1/3))
   *
   */
  P.cubeRoot = P.cbrt = function () {
    var e, m, n, r, rep, s, sd, t, t3, t3plusx,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite() || x.isZero()) return new Ctor(x);
    external = false;

    // Initial estimate.
    s = x.s * Math.pow(x.s * x, 1 / 3);

     // Math.cbrt underflow/overflow?
     // Pass x to Math.pow as integer, then adjust the exponent of the result.
    if (!s || Math.abs(s) == 1 / 0) {
      n = digitsToString(x.d);
      e = x.e;

      // Adjust n exponent so it is a multiple of 3 away from x exponent.
      if (s = (e - n.length + 1) % 3) n += (s == 1 || s == -2 ? '0' : '00');
      s = Math.pow(n, 1 / 3);

      // Rarely, e may be one less than the result exponent value.
      e = mathfloor((e + 1) / 3) - (e % 3 == (e < 0 ? -1 : 2));

      if (s == 1 / 0) {
        n = '5e' + e;
      } else {
        n = s.toExponential();
        n = n.slice(0, n.indexOf('e') + 1) + e;
      }

      r = new Ctor(n);
      r.s = x.s;
    } else {
      r = new Ctor(s.toString());
    }

    sd = (e = Ctor.precision) + 3;

    // Halley's method.
    // TODO? Compare Newton's method.
    for (;;) {
      t = r;
      t3 = t.times(t).times(t);
      t3plusx = t3.plus(x);
      r = divide(t3plusx.plus(x).times(t), t3plusx.plus(t3), sd + 2, 1);

      // TODO? Replace with for-loop and checkRoundingDigits.
      if (digitsToString(t.d).slice(0, sd) === (n = digitsToString(r.d)).slice(0, sd)) {
        n = n.slice(sd - 3, sd + 1);

        // The 4th rounding digit may be in error by -1 so if the 4 rounding digits are 9999 or 4999
        // , i.e. approaching a rounding boundary, continue the iteration.
        if (n == '9999' || !rep && n == '4999') {

          // On the first iteration only, check to see if rounding up gives the exact result as the
          // nines may infinitely repeat.
          if (!rep) {
            finalise(t, e + 1, 0);

            if (t.times(t).times(t).eq(x)) {
              r = t;
              break;
            }
          }

          sd += 4;
          rep = 1;
        } else {

          // If the rounding digits are null, 0{0,4} or 50{0,3}, check for an exact result.
          // If not, then there are further digits and m will be truthy.
          if (!+n || !+n.slice(1) && n.charAt(0) == '5') {

            // Truncate to the first rounding digit.
            finalise(r, e + 1, 1);
            m = !r.times(r).times(r).eq(x);
          }

          break;
        }
      }
    }

    external = true;

    return finalise(r, e, Ctor.rounding, m);
  };


  /*
   * Return the number of decimal places of the value of this Decimal.
   *
   */
  P.decimalPlaces = P.dp = function () {
    var w,
      d = this.d,
      n = NaN;

    if (d) {
      w = d.length - 1;
      n = (w - mathfloor(this.e / LOG_BASE)) * LOG_BASE;

      // Subtract the number of trailing zeros of the last word.
      w = d[w];
      if (w) for (; w % 10 == 0; w /= 10) n--;
      if (n < 0) n = 0;
    }

    return n;
  };


  /*
   *  n / 0 = I
   *  n / N = N
   *  n / I = 0
   *  0 / n = 0
   *  0 / 0 = N
   *  0 / N = N
   *  0 / I = 0
   *  N / n = N
   *  N / 0 = N
   *  N / N = N
   *  N / I = N
   *  I / n = I
   *  I / 0 = I
   *  I / N = N
   *  I / I = N
   *
   * Return a new Decimal whose value is the value of this Decimal divided by `y`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   */
  P.dividedBy = P.div = function (y) {
    return divide(this, new this.constructor(y));
  };


  /*
   * Return a new Decimal whose value is the integer part of dividing the value of this Decimal
   * by the value of `y`, rounded to `precision` significant digits using rounding mode `rounding`.
   *
   */
  P.dividedToIntegerBy = P.divToInt = function (y) {
    var x = this,
      Ctor = x.constructor;
    return finalise(divide(x, new Ctor(y), 0, 1, 1), Ctor.precision, Ctor.rounding);
  };


  /*
   * Return true if the value of this Decimal is equal to the value of `y`, otherwise return false.
   *
   */
  P.equals = P.eq = function (y) {
    return this.cmp(y) === 0;
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a whole number in the
   * direction of negative Infinity.
   *
   */
  P.floor = function () {
    return finalise(new this.constructor(this), this.e + 1, 3);
  };


  /*
   * Return true if the value of this Decimal is greater than the value of `y`, otherwise return
   * false.
   *
   */
  P.greaterThan = P.gt = function (y) {
    return this.cmp(y) > 0;
  };


  /*
   * Return true if the value of this Decimal is greater than or equal to the value of `y`,
   * otherwise return false.
   *
   */
  P.greaterThanOrEqualTo = P.gte = function (y) {
    var k = this.cmp(y);
    return k == 1 || k === 0;
  };


  /*
   * Return a new Decimal whose value is the hyperbolic cosine of the value in radians of this
   * Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [1, Infinity]
   *
   * cosh(x) = 1 + x^2/2! + x^4/4! + x^6/6! + ...
   *
   * cosh(0)         = 1
   * cosh(-0)        = 1
   * cosh(Infinity)  = Infinity
   * cosh(-Infinity) = Infinity
   * cosh(NaN)       = NaN
   *
   *  x        time taken (ms)   result
   * 1000      9                 9.8503555700852349694e+433
   * 10000     25                4.4034091128314607936e+4342
   * 100000    171               1.4033316802130615897e+43429
   * 1000000   3817              1.5166076984010437725e+434294
   * 10000000  abandoned after 2 minute wait
   *
   * TODO? Compare performance of cosh(x) = 0.5 * (exp(x) + exp(-x))
   *
   */
  P.hyperbolicCosine = P.cosh = function () {
    var k, n, pr, rm, len,
      x = this,
      Ctor = x.constructor,
      one = new Ctor(1);

    if (!x.isFinite()) return new Ctor(x.s ? 1 / 0 : NaN);
    if (x.isZero()) return one;

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(x.e, x.sd()) + 4;
    Ctor.rounding = 1;
    len = x.d.length;

    // Argument reduction: cos(4x) = 1 - 8cos^2(x) + 8cos^4(x) + 1
    // i.e. cos(x) = 1 - cos^2(x/4)(8 - 8cos^2(x/4))

    // Estimate the optimum number of times to use the argument reduction.
    // TODO? Estimation reused from cosine() and may not be optimal here.
    if (len < 32) {
      k = Math.ceil(len / 3);
      n = Math.pow(4, -k).toString();
    } else {
      k = 16;
      n = '2.3283064365386962890625e-10';
    }

    x = taylorSeries(Ctor, 1, x.times(n), new Ctor(1), true);

    // Reverse argument reduction
    var cosh2_x,
      i = k,
      d8 = new Ctor(8);
    for (; i--;) {
      cosh2_x = x.times(x);
      x = one.minus(cosh2_x.times(d8.minus(cosh2_x.times(d8))));
    }

    return finalise(x, Ctor.precision = pr, Ctor.rounding = rm, true);
  };


  /*
   * Return a new Decimal whose value is the hyperbolic sine of the value in radians of this
   * Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-Infinity, Infinity]
   *
   * sinh(x) = x + x^3/3! + x^5/5! + x^7/7! + ...
   *
   * sinh(0)         = 0
   * sinh(-0)        = -0
   * sinh(Infinity)  = Infinity
   * sinh(-Infinity) = -Infinity
   * sinh(NaN)       = NaN
   *
   * x        time taken (ms)
   * 10       2 ms
   * 100      5 ms
   * 1000     14 ms
   * 10000    82 ms
   * 100000   886 ms            1.4033316802130615897e+43429
   * 200000   2613 ms
   * 300000   5407 ms
   * 400000   8824 ms
   * 500000   13026 ms          8.7080643612718084129e+217146
   * 1000000  48543 ms
   *
   * TODO? Compare performance of sinh(x) = 0.5 * (exp(x) - exp(-x))
   *
   */
  P.hyperbolicSine = P.sinh = function () {
    var k, pr, rm, len,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite() || x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(x.e, x.sd()) + 4;
    Ctor.rounding = 1;
    len = x.d.length;

    if (len < 3) {
      x = taylorSeries(Ctor, 2, x, x, true);
    } else {

      // Alternative argument reduction: sinh(3x) = sinh(x)(3 + 4sinh^2(x))
      // i.e. sinh(x) = sinh(x/3)(3 + 4sinh^2(x/3))
      // 3 multiplications and 1 addition

      // Argument reduction: sinh(5x) = sinh(x)(5 + sinh^2(x)(20 + 16sinh^2(x)))
      // i.e. sinh(x) = sinh(x/5)(5 + sinh^2(x/5)(20 + 16sinh^2(x/5)))
      // 4 multiplications and 2 additions

      // Estimate the optimum number of times to use the argument reduction.
      k = 1.4 * Math.sqrt(len);
      k = k > 16 ? 16 : k | 0;

      x = x.times(Math.pow(5, -k));

      x = taylorSeries(Ctor, 2, x, x, true);

      // Reverse argument reduction
      var sinh2_x,
        d5 = new Ctor(5),
        d16 = new Ctor(16),
        d20 = new Ctor(20);
      for (; k--;) {
        sinh2_x = x.times(x);
        x = x.times(d5.plus(sinh2_x.times(d16.times(sinh2_x).plus(d20))));
      }
    }

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return finalise(x, pr, rm, true);
  };


  /*
   * Return a new Decimal whose value is the hyperbolic tangent of the value in radians of this
   * Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-1, 1]
   *
   * tanh(x) = sinh(x) / cosh(x)
   *
   * tanh(0)         = 0
   * tanh(-0)        = -0
   * tanh(Infinity)  = 1
   * tanh(-Infinity) = -1
   * tanh(NaN)       = NaN
   *
   */
  P.hyperbolicTangent = P.tanh = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite()) return new Ctor(x.s);
    if (x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + 7;
    Ctor.rounding = 1;

    return divide(x.sinh(), x.cosh(), Ctor.precision = pr, Ctor.rounding = rm);
  };


  /*
   * Return a new Decimal whose value is the arccosine (inverse cosine) in radians of the value of
   * this Decimal.
   *
   * Domain: [-1, 1]
   * Range: [0, pi]
   *
   * acos(x) = pi/2 - asin(x)
   *
   * acos(0)       = pi/2
   * acos(-0)      = pi/2
   * acos(1)       = 0
   * acos(-1)      = pi
   * acos(1/2)     = pi/3
   * acos(-1/2)    = 2*pi/3
   * acos(|x| > 1) = NaN
   * acos(NaN)     = NaN
   *
   */
  P.inverseCosine = P.acos = function () {
    var halfPi,
      x = this,
      Ctor = x.constructor,
      k = x.abs().cmp(1),
      pr = Ctor.precision,
      rm = Ctor.rounding;

    if (k !== -1) {
      return k === 0
        // |x| is 1
        ? x.isNeg() ? getPi(Ctor, pr, rm) : new Ctor(0)
        // |x| > 1 or x is NaN
        : new Ctor(NaN);
    }

    if (x.isZero()) return getPi(Ctor, pr + 4, rm).times(0.5);

    // TODO? Special case acos(0.5) = pi/3 and acos(-0.5) = 2*pi/3

    Ctor.precision = pr + 6;
    Ctor.rounding = 1;

    x = x.asin();
    halfPi = getPi(Ctor, pr + 4, rm).times(0.5);

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return halfPi.minus(x);
  };


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic cosine in radians of the
   * value of this Decimal.
   *
   * Domain: [1, Infinity]
   * Range: [0, Infinity]
   *
   * acosh(x) = ln(x + sqrt(x^2 - 1))
   *
   * acosh(x < 1)     = NaN
   * acosh(NaN)       = NaN
   * acosh(Infinity)  = Infinity
   * acosh(-Infinity) = NaN
   * acosh(0)         = NaN
   * acosh(-0)        = NaN
   * acosh(1)         = 0
   * acosh(-1)        = NaN
   *
   */
  P.inverseHyperbolicCosine = P.acosh = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (x.lte(1)) return new Ctor(x.eq(1) ? 0 : NaN);
    if (!x.isFinite()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(Math.abs(x.e), x.sd()) + 4;
    Ctor.rounding = 1;
    external = false;

    x = x.times(x).minus(1).sqrt().plus(x);

    external = true;
    Ctor.precision = pr;
    Ctor.rounding = rm;

    return x.ln();
  };


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic sine in radians of the value
   * of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-Infinity, Infinity]
   *
   * asinh(x) = ln(x + sqrt(x^2 + 1))
   *
   * asinh(NaN)       = NaN
   * asinh(Infinity)  = Infinity
   * asinh(-Infinity) = -Infinity
   * asinh(0)         = 0
   * asinh(-0)        = -0
   *
   */
  P.inverseHyperbolicSine = P.asinh = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite() || x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + 2 * Math.max(Math.abs(x.e), x.sd()) + 6;
    Ctor.rounding = 1;
    external = false;

    x = x.times(x).plus(1).sqrt().plus(x);

    external = true;
    Ctor.precision = pr;
    Ctor.rounding = rm;

    return x.ln();
  };


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic tangent in radians of the
   * value of this Decimal.
   *
   * Domain: [-1, 1]
   * Range: [-Infinity, Infinity]
   *
   * atanh(x) = 0.5 * ln((1 + x) / (1 - x))
   *
   * atanh(|x| > 1)   = NaN
   * atanh(NaN)       = NaN
   * atanh(Infinity)  = NaN
   * atanh(-Infinity) = NaN
   * atanh(0)         = 0
   * atanh(-0)        = -0
   * atanh(1)         = Infinity
   * atanh(-1)        = -Infinity
   *
   */
  P.inverseHyperbolicTangent = P.atanh = function () {
    var pr, rm, wpr, xsd,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite()) return new Ctor(NaN);
    if (x.e >= 0) return new Ctor(x.abs().eq(1) ? x.s / 0 : x.isZero() ? x : NaN);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    xsd = x.sd();

    if (Math.max(xsd, pr) < 2 * -x.e - 1) return finalise(new Ctor(x), pr, rm, true);

    Ctor.precision = wpr = xsd - x.e;

    x = divide(x.plus(1), new Ctor(1).minus(x), wpr + pr, 1);

    Ctor.precision = pr + 4;
    Ctor.rounding = 1;

    x = x.ln();

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return x.times(0.5);
  };


  /*
   * Return a new Decimal whose value is the arcsine (inverse sine) in radians of the value of this
   * Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-pi/2, pi/2]
   *
   * asin(x) = 2*atan(x/(1 + sqrt(1 - x^2)))
   *
   * asin(0)       = 0
   * asin(-0)      = -0
   * asin(1/2)     = pi/6
   * asin(-1/2)    = -pi/6
   * asin(1)       = pi/2
   * asin(-1)      = -pi/2
   * asin(|x| > 1) = NaN
   * asin(NaN)     = NaN
   *
   * TODO? Compare performance of Taylor series.
   *
   */
  P.inverseSine = P.asin = function () {
    var halfPi, k,
      pr, rm,
      x = this,
      Ctor = x.constructor;

    if (x.isZero()) return new Ctor(x);

    k = x.abs().cmp(1);
    pr = Ctor.precision;
    rm = Ctor.rounding;

    if (k !== -1) {

      // |x| is 1
      if (k === 0) {
        halfPi = getPi(Ctor, pr + 4, rm).times(0.5);
        halfPi.s = x.s;
        return halfPi;
      }

      // |x| > 1 or x is NaN
      return new Ctor(NaN);
    }

    // TODO? Special case asin(1/2) = pi/6 and asin(-1/2) = -pi/6

    Ctor.precision = pr + 6;
    Ctor.rounding = 1;

    x = x.div(new Ctor(1).minus(x.times(x)).sqrt().plus(1)).atan();

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return x.times(2);
  };


  /*
   * Return a new Decimal whose value is the arctangent (inverse tangent) in radians of the value
   * of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-pi/2, pi/2]
   *
   * atan(x) = x - x^3/3 + x^5/5 - x^7/7 + ...
   *
   * atan(0)         = 0
   * atan(-0)        = -0
   * atan(1)         = pi/4
   * atan(-1)        = -pi/4
   * atan(Infinity)  = pi/2
   * atan(-Infinity) = -pi/2
   * atan(NaN)       = NaN
   *
   */
  P.inverseTangent = P.atan = function () {
    var i, j, k, n, px, t, r, wpr, x2,
      x = this,
      Ctor = x.constructor,
      pr = Ctor.precision,
      rm = Ctor.rounding;

    if (!x.isFinite()) {
      if (!x.s) return new Ctor(NaN);
      if (pr + 4 <= PI_PRECISION) {
        r = getPi(Ctor, pr + 4, rm).times(0.5);
        r.s = x.s;
        return r;
      }
    } else if (x.isZero()) {
      return new Ctor(x);
    } else if (x.abs().eq(1) && pr + 4 <= PI_PRECISION) {
      r = getPi(Ctor, pr + 4, rm).times(0.25);
      r.s = x.s;
      return r;
    }

    Ctor.precision = wpr = pr + 10;
    Ctor.rounding = 1;

    // TODO? if (x >= 1 && pr <= PI_PRECISION) atan(x) = halfPi * x.s - atan(1 / x);

    // Argument reduction
    // Ensure |x| < 0.42
    // atan(x) = 2 * atan(x / (1 + sqrt(1 + x^2)))

    k = Math.min(28, wpr / LOG_BASE + 2 | 0);

    for (i = k; i; --i) x = x.div(x.times(x).plus(1).sqrt().plus(1));

    external = false;

    j = Math.ceil(wpr / LOG_BASE);
    n = 1;
    x2 = x.times(x);
    r = new Ctor(x);
    px = x;

    // atan(x) = x - x^3/3 + x^5/5 - x^7/7 + ...
    for (; i !== -1;) {
      px = px.times(x2);
      t = r.minus(px.div(n += 2));

      px = px.times(x2);
      r = t.plus(px.div(n += 2));

      if (r.d[j] !== void 0) for (i = j; r.d[i] === t.d[i] && i--;);
    }

    if (k) r = r.times(2 << (k - 1));

    external = true;

    return finalise(r, Ctor.precision = pr, Ctor.rounding = rm, true);
  };


  /*
   * Return true if the value of this Decimal is a finite number, otherwise return false.
   *
   */
  P.isFinite = function () {
    return !!this.d;
  };


  /*
   * Return true if the value of this Decimal is an integer, otherwise return false.
   *
   */
  P.isInteger = P.isInt = function () {
    return !!this.d && mathfloor(this.e / LOG_BASE) > this.d.length - 2;
  };


  /*
   * Return true if the value of this Decimal is NaN, otherwise return false.
   *
   */
  P.isNaN = function () {
    return !this.s;
  };


  /*
   * Return true if the value of this Decimal is negative, otherwise return false.
   *
   */
  P.isNegative = P.isNeg = function () {
    return this.s < 0;
  };


  /*
   * Return true if the value of this Decimal is positive, otherwise return false.
   *
   */
  P.isPositive = P.isPos = function () {
    return this.s > 0;
  };


  /*
   * Return true if the value of this Decimal is 0 or -0, otherwise return false.
   *
   */
  P.isZero = function () {
    return !!this.d && this.d[0] === 0;
  };


  /*
   * Return true if the value of this Decimal is less than `y`, otherwise return false.
   *
   */
  P.lessThan = P.lt = function (y) {
    return this.cmp(y) < 0;
  };


  /*
   * Return true if the value of this Decimal is less than or equal to `y`, otherwise return false.
   *
   */
  P.lessThanOrEqualTo = P.lte = function (y) {
    return this.cmp(y) < 1;
  };


  /*
   * Return the logarithm of the value of this Decimal to the specified base, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * If no base is specified, return log[10](arg).
   *
   * log[base](arg) = ln(arg) / ln(base)
   *
   * The result will always be correctly rounded if the base of the log is 10, and 'almost always'
   * otherwise:
   *
   * Depending on the rounding mode, the result may be incorrectly rounded if the first fifteen
   * rounding digits are [49]99999999999999 or [50]00000000000000. In that case, the maximum error
   * between the result and the correctly rounded result will be one ulp (unit in the last place).
   *
   * log[-b](a)       = NaN
   * log[0](a)        = NaN
   * log[1](a)        = NaN
   * log[NaN](a)      = NaN
   * log[Infinity](a) = NaN
   * log[b](0)        = -Infinity
   * log[b](-0)       = -Infinity
   * log[b](-a)       = NaN
   * log[b](1)        = 0
   * log[b](Infinity) = Infinity
   * log[b](NaN)      = NaN
   *
   * [base] {number|string|Decimal} The base of the logarithm.
   *
   */
  P.logarithm = P.log = function (base) {
    var isBase10, d, denominator, k, inf, num, sd, r,
      arg = this,
      Ctor = arg.constructor,
      pr = Ctor.precision,
      rm = Ctor.rounding,
      guard = 5;

    // Default base is 10.
    if (base == null) {
      base = new Ctor(10);
      isBase10 = true;
    } else {
      base = new Ctor(base);
      d = base.d;

      // Return NaN if base is negative, or non-finite, or is 0 or 1.
      if (base.s < 0 || !d || !d[0] || base.eq(1)) return new Ctor(NaN);

      isBase10 = base.eq(10);
    }

    d = arg.d;

    // Is arg negative, non-finite, 0 or 1?
    if (arg.s < 0 || !d || !d[0] || arg.eq(1)) {
      return new Ctor(d && !d[0] ? -1 / 0 : arg.s != 1 ? NaN : d ? 0 : 1 / 0);
    }

    // The result will have a non-terminating decimal expansion if base is 10 and arg is not an
    // integer power of 10.
    if (isBase10) {
      if (d.length > 1) {
        inf = true;
      } else {
        for (k = d[0]; k % 10 === 0;) k /= 10;
        inf = k !== 1;
      }
    }

    external = false;
    sd = pr + guard;
    num = naturalLogarithm(arg, sd);
    denominator = isBase10 ? getLn10(Ctor, sd + 10) : naturalLogarithm(base, sd);

    // The result will have 5 rounding digits.
    r = divide(num, denominator, sd, 1);

    // If at a rounding boundary, i.e. the result's rounding digits are [49]9999 or [50]0000,
    // calculate 10 further digits.
    //
    // If the result is known to have an infinite decimal expansion, repeat this until it is clear
    // that the result is above or below the boundary. Otherwise, if after calculating the 10
    // further digits, the last 14 are nines, round up and assume the result is exact.
    // Also assume the result is exact if the last 14 are zero.
    //
    // Example of a result that will be incorrectly rounded:
    // log[1048576](4503599627370502) = 2.60000000000000009610279511444746...
    // The above result correctly rounded using ROUND_CEIL to 1 decimal place should be 2.7, but it
    // will be given as 2.6 as there are 15 zeros immediately after the requested decimal place, so
    // the exact result would be assumed to be 2.6, which rounded using ROUND_CEIL to 1 decimal
    // place is still 2.6.
    if (checkRoundingDigits(r.d, k = pr, rm)) {

      do {
        sd += 10;
        num = naturalLogarithm(arg, sd);
        denominator = isBase10 ? getLn10(Ctor, sd + 10) : naturalLogarithm(base, sd);
        r = divide(num, denominator, sd, 1);

        if (!inf) {

          // Check for 14 nines from the 2nd rounding digit, as the first may be 4.
          if (+digitsToString(r.d).slice(k + 1, k + 15) + 1 == 1e14) {
            r = finalise(r, pr + 1, 0);
          }

          break;
        }
      } while (checkRoundingDigits(r.d, k += 10, rm));
    }

    external = true;

    return finalise(r, pr, rm);
  };


  /*
   * Return a new Decimal whose value is the maximum of the arguments and the value of this Decimal.
   *
   * arguments {number|string|Decimal}
   *
  P.max = function () {
    Array.prototype.push.call(arguments, this);
    return maxOrMin(this.constructor, arguments, 'lt');
  };
   */


  /*
   * Return a new Decimal whose value is the minimum of the arguments and the value of this Decimal.
   *
   * arguments {number|string|Decimal}
   *
  P.min = function () {
    Array.prototype.push.call(arguments, this);
    return maxOrMin(this.constructor, arguments, 'gt');
  };
   */


  /*
   *  n - 0 = n
   *  n - N = N
   *  n - I = -I
   *  0 - n = -n
   *  0 - 0 = 0
   *  0 - N = N
   *  0 - I = -I
   *  N - n = N
   *  N - 0 = N
   *  N - N = N
   *  N - I = N
   *  I - n = I
   *  I - 0 = I
   *  I - N = N
   *  I - I = N
   *
   * Return a new Decimal whose value is the value of this Decimal minus `y`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   */
  P.minus = P.sub = function (y) {
    var d, e, i, j, k, len, pr, rm, xd, xe, xLTy, yd,
      x = this,
      Ctor = x.constructor;

    y = new Ctor(y);

    // If either is not finite...
    if (!x.d || !y.d) {

      // Return NaN if either is NaN.
      if (!x.s || !y.s) y = new Ctor(NaN);

      // Return y negated if x is finite and y is ±Infinity.
      else if (x.d) y.s = -y.s;

      // Return x if y is finite and x is ±Infinity.
      // Return x if both are ±Infinity with different signs.
      // Return NaN if both are ±Infinity with the same sign.
      else y = new Ctor(y.d || x.s !== y.s ? x : NaN);

      return y;
    }

    // If signs differ...
    if (x.s != y.s) {
      y.s = -y.s;
      return x.plus(y);
    }

    xd = x.d;
    yd = y.d;
    pr = Ctor.precision;
    rm = Ctor.rounding;

    // If either is zero...
    if (!xd[0] || !yd[0]) {

      // Return y negated if x is zero and y is non-zero.
      if (yd[0]) y.s = -y.s;

      // Return x if y is zero and x is non-zero.
      else if (xd[0]) y = new Ctor(x);

      // Return zero if both are zero.
      // From IEEE 754 (2008) 6.3: 0 - 0 = -0 - -0 = -0 when rounding to -Infinity.
      else return new Ctor(rm === 3 ? -0 : 0);

      return external ? finalise(y, pr, rm) : y;
    }

    // x and y are finite, non-zero numbers with the same sign.

    // Calculate base 1e7 exponents.
    e = mathfloor(y.e / LOG_BASE);
    xe = mathfloor(x.e / LOG_BASE);

    xd = xd.slice();
    k = xe - e;

    // If base 1e7 exponents differ...
    if (k) {
      xLTy = k < 0;

      if (xLTy) {
        d = xd;
        k = -k;
        len = yd.length;
      } else {
        d = yd;
        e = xe;
        len = xd.length;
      }

      // Numbers with massively different exponents would result in a very high number of
      // zeros needing to be prepended, but this can be avoided while still ensuring correct
      // rounding by limiting the number of zeros to `Math.ceil(pr / LOG_BASE) + 2`.
      i = Math.max(Math.ceil(pr / LOG_BASE), len) + 2;

      if (k > i) {
        k = i;
        d.length = 1;
      }

      // Prepend zeros to equalise exponents.
      d.reverse();
      for (i = k; i--;) d.push(0);
      d.reverse();

    // Base 1e7 exponents equal.
    } else {

      // Check digits to determine which is the bigger number.

      i = xd.length;
      len = yd.length;
      xLTy = i < len;
      if (xLTy) len = i;

      for (i = 0; i < len; i++) {
        if (xd[i] != yd[i]) {
          xLTy = xd[i] < yd[i];
          break;
        }
      }

      k = 0;
    }

    if (xLTy) {
      d = xd;
      xd = yd;
      yd = d;
      y.s = -y.s;
    }

    len = xd.length;

    // Append zeros to `xd` if shorter.
    // Don't add zeros to `yd` if shorter as subtraction only needs to start at `yd` length.
    for (i = yd.length - len; i > 0; --i) xd[len++] = 0;

    // Subtract yd from xd.
    for (i = yd.length; i > k;) {

      if (xd[--i] < yd[i]) {
        for (j = i; j && xd[--j] === 0;) xd[j] = BASE - 1;
        --xd[j];
        xd[i] += BASE;
      }

      xd[i] -= yd[i];
    }

    // Remove trailing zeros.
    for (; xd[--len] === 0;) xd.pop();

    // Remove leading zeros and adjust exponent accordingly.
    for (; xd[0] === 0; xd.shift()) --e;

    // Zero?
    if (!xd[0]) return new Ctor(rm === 3 ? -0 : 0);

    y.d = xd;
    y.e = getBase10Exponent(xd, e);

    return external ? finalise(y, pr, rm) : y;
  };


  /*
   *   n % 0 =  N
   *   n % N =  N
   *   n % I =  n
   *   0 % n =  0
   *  -0 % n = -0
   *   0 % 0 =  N
   *   0 % N =  N
   *   0 % I =  0
   *   N % n =  N
   *   N % 0 =  N
   *   N % N =  N
   *   N % I =  N
   *   I % n =  N
   *   I % 0 =  N
   *   I % N =  N
   *   I % I =  N
   *
   * Return a new Decimal whose value is the value of this Decimal modulo `y`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   * The result depends on the modulo mode.
   *
   */
  P.modulo = P.mod = function (y) {
    var q,
      x = this,
      Ctor = x.constructor;

    y = new Ctor(y);

    // Return NaN if x is ±Infinity or NaN, or y is NaN or ±0.
    if (!x.d || !y.s || y.d && !y.d[0]) return new Ctor(NaN);

    // Return x if y is ±Infinity or x is ±0.
    if (!y.d || x.d && !x.d[0]) {
      return finalise(new Ctor(x), Ctor.precision, Ctor.rounding);
    }

    // Prevent rounding of intermediate calculations.
    external = false;

    if (Ctor.modulo == 9) {

      // Euclidian division: q = sign(y) * floor(x / abs(y))
      // result = x - q * y    where  0 <= result < abs(y)
      q = divide(x, y.abs(), 0, 3, 1);
      q.s *= y.s;
    } else {
      q = divide(x, y, 0, Ctor.modulo, 1);
    }

    q = q.times(y);

    external = true;

    return x.minus(q);
  };


  /*
   * Return a new Decimal whose value is the natural exponential of the value of this Decimal,
   * i.e. the base e raised to the power the value of this Decimal, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   */
  P.naturalExponential = P.exp = function () {
    return naturalExponential(this);
  };


  /*
   * Return a new Decimal whose value is the natural logarithm of the value of this Decimal,
   * rounded to `precision` significant digits using rounding mode `rounding`.
   *
   */
  P.naturalLogarithm = P.ln = function () {
    return naturalLogarithm(this);
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal negated, i.e. as if multiplied by
   * -1.
   *
   */
  P.negated = P.neg = function () {
    var x = new this.constructor(this);
    x.s = -x.s;
    return finalise(x);
  };


  /*
   *  n + 0 = n
   *  n + N = N
   *  n + I = I
   *  0 + n = n
   *  0 + 0 = 0
   *  0 + N = N
   *  0 + I = I
   *  N + n = N
   *  N + 0 = N
   *  N + N = N
   *  N + I = N
   *  I + n = I
   *  I + 0 = I
   *  I + N = N
   *  I + I = I
   *
   * Return a new Decimal whose value is the value of this Decimal plus `y`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   */
  P.plus = P.add = function (y) {
    var carry, d, e, i, k, len, pr, rm, xd, yd,
      x = this,
      Ctor = x.constructor;

    y = new Ctor(y);

    // If either is not finite...
    if (!x.d || !y.d) {

      // Return NaN if either is NaN.
      if (!x.s || !y.s) y = new Ctor(NaN);

      // Return x if y is finite and x is ±Infinity.
      // Return x if both are ±Infinity with the same sign.
      // Return NaN if both are ±Infinity with different signs.
      // Return y if x is finite and y is ±Infinity.
      else if (!x.d) y = new Ctor(y.d || x.s === y.s ? x : NaN);

      return y;
    }

     // If signs differ...
    if (x.s != y.s) {
      y.s = -y.s;
      return x.minus(y);
    }

    xd = x.d;
    yd = y.d;
    pr = Ctor.precision;
    rm = Ctor.rounding;

    // If either is zero...
    if (!xd[0] || !yd[0]) {

      // Return x if y is zero.
      // Return y if y is non-zero.
      if (!yd[0]) y = new Ctor(x);

      return external ? finalise(y, pr, rm) : y;
    }

    // x and y are finite, non-zero numbers with the same sign.

    // Calculate base 1e7 exponents.
    k = mathfloor(x.e / LOG_BASE);
    e = mathfloor(y.e / LOG_BASE);

    xd = xd.slice();
    i = k - e;

    // If base 1e7 exponents differ...
    if (i) {

      if (i < 0) {
        d = xd;
        i = -i;
        len = yd.length;
      } else {
        d = yd;
        e = k;
        len = xd.length;
      }

      // Limit number of zeros prepended to max(ceil(pr / LOG_BASE), len) + 1.
      k = Math.ceil(pr / LOG_BASE);
      len = k > len ? k + 1 : len + 1;

      if (i > len) {
        i = len;
        d.length = 1;
      }

      // Prepend zeros to equalise exponents. Note: Faster to use reverse then do unshifts.
      d.reverse();
      for (; i--;) d.push(0);
      d.reverse();
    }

    len = xd.length;
    i = yd.length;

    // If yd is longer than xd, swap xd and yd so xd points to the longer array.
    if (len - i < 0) {
      i = len;
      d = yd;
      yd = xd;
      xd = d;
    }

    // Only start adding at yd.length - 1 as the further digits of xd can be left as they are.
    for (carry = 0; i;) {
      carry = (xd[--i] = xd[i] + yd[i] + carry) / BASE | 0;
      xd[i] %= BASE;
    }

    if (carry) {
      xd.unshift(carry);
      ++e;
    }

    // Remove trailing zeros.
    // No need to check for zero, as +x + +y != 0 && -x + -y != 0
    for (len = xd.length; xd[--len] == 0;) xd.pop();

    y.d = xd;
    y.e = getBase10Exponent(xd, e);

    return external ? finalise(y, pr, rm) : y;
  };


  /*
   * Return the number of significant digits of the value of this Decimal.
   *
   * [z] {boolean|number} Whether to count integer-part trailing zeros: true, false, 1 or 0.
   *
   */
  P.precision = P.sd = function (z) {
    var k,
      x = this;

    if (z !== void 0 && z !== !!z && z !== 1 && z !== 0) throw Error(invalidArgument + z);

    if (x.d) {
      k = getPrecision(x.d);
      if (z && x.e + 1 > k) k = x.e + 1;
    } else {
      k = NaN;
    }

    return k;
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a whole number using
   * rounding mode `rounding`.
   *
   */
  P.round = function () {
    var x = this,
      Ctor = x.constructor;

    return finalise(new Ctor(x), x.e + 1, Ctor.rounding);
  };


  /*
   * Return a new Decimal whose value is the sine of the value in radians of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-1, 1]
   *
   * sin(x) = x - x^3/3! + x^5/5! - ...
   *
   * sin(0)         = 0
   * sin(-0)        = -0
   * sin(Infinity)  = NaN
   * sin(-Infinity) = NaN
   * sin(NaN)       = NaN
   *
   */
  P.sine = P.sin = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite()) return new Ctor(NaN);
    if (x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + Math.max(x.e, x.sd()) + LOG_BASE;
    Ctor.rounding = 1;

    x = sine(Ctor, toLessThanHalfPi(Ctor, x));

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return finalise(quadrant > 2 ? x.neg() : x, pr, rm, true);
  };


  /*
   * Return a new Decimal whose value is the square root of this Decimal, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   *  sqrt(-n) =  N
   *  sqrt(N)  =  N
   *  sqrt(-I) =  N
   *  sqrt(I)  =  I
   *  sqrt(0)  =  0
   *  sqrt(-0) = -0
   *
   */
  P.squareRoot = P.sqrt = function () {
    var m, n, sd, r, rep, t,
      x = this,
      d = x.d,
      e = x.e,
      s = x.s,
      Ctor = x.constructor;

    // Negative/NaN/Infinity/zero?
    if (s !== 1 || !d || !d[0]) {
      return new Ctor(!s || s < 0 && (!d || d[0]) ? NaN : d ? x : 1 / 0);
    }

    external = false;

    // Initial estimate.
    s = Math.sqrt(+x);

    // Math.sqrt underflow/overflow?
    // Pass x to Math.sqrt as integer, then adjust the exponent of the result.
    if (s == 0 || s == 1 / 0) {
      n = digitsToString(d);

      if ((n.length + e) % 2 == 0) n += '0';
      s = Math.sqrt(n);
      e = mathfloor((e + 1) / 2) - (e < 0 || e % 2);

      if (s == 1 / 0) {
        n = '1e' + e;
      } else {
        n = s.toExponential();
        n = n.slice(0, n.indexOf('e') + 1) + e;
      }

      r = new Ctor(n);
    } else {
      r = new Ctor(s.toString());
    }

    sd = (e = Ctor.precision) + 3;

    // Newton-Raphson iteration.
    for (;;) {
      t = r;
      r = t.plus(divide(x, t, sd + 2, 1)).times(0.5);

      // TODO? Replace with for-loop and checkRoundingDigits.
      if (digitsToString(t.d).slice(0, sd) === (n = digitsToString(r.d)).slice(0, sd)) {
        n = n.slice(sd - 3, sd + 1);

        // The 4th rounding digit may be in error by -1 so if the 4 rounding digits are 9999 or
        // 4999, i.e. approaching a rounding boundary, continue the iteration.
        if (n == '9999' || !rep && n == '4999') {

          // On the first iteration only, check to see if rounding up gives the exact result as the
          // nines may infinitely repeat.
          if (!rep) {
            finalise(t, e + 1, 0);

            if (t.times(t).eq(x)) {
              r = t;
              break;
            }
          }

          sd += 4;
          rep = 1;
        } else {

          // If the rounding digits are null, 0{0,4} or 50{0,3}, check for an exact result.
          // If not, then there are further digits and m will be truthy.
          if (!+n || !+n.slice(1) && n.charAt(0) == '5') {

            // Truncate to the first rounding digit.
            finalise(r, e + 1, 1);
            m = !r.times(r).eq(x);
          }

          break;
        }
      }
    }

    external = true;

    return finalise(r, e, Ctor.rounding, m);
  };


  /*
   * Return a new Decimal whose value is the tangent of the value in radians of this Decimal.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-Infinity, Infinity]
   *
   * tan(0)         = 0
   * tan(-0)        = -0
   * tan(Infinity)  = NaN
   * tan(-Infinity) = NaN
   * tan(NaN)       = NaN
   *
   */
  P.tangent = P.tan = function () {
    var pr, rm,
      x = this,
      Ctor = x.constructor;

    if (!x.isFinite()) return new Ctor(NaN);
    if (x.isZero()) return new Ctor(x);

    pr = Ctor.precision;
    rm = Ctor.rounding;
    Ctor.precision = pr + 10;
    Ctor.rounding = 1;

    x = x.sin();
    x.s = 1;
    x = divide(x, new Ctor(1).minus(x.times(x)).sqrt(), pr + 10, 0);

    Ctor.precision = pr;
    Ctor.rounding = rm;

    return finalise(quadrant == 2 || quadrant == 4 ? x.neg() : x, pr, rm, true);
  };


  /*
   *  n * 0 = 0
   *  n * N = N
   *  n * I = I
   *  0 * n = 0
   *  0 * 0 = 0
   *  0 * N = N
   *  0 * I = N
   *  N * n = N
   *  N * 0 = N
   *  N * N = N
   *  N * I = N
   *  I * n = I
   *  I * 0 = N
   *  I * N = N
   *  I * I = I
   *
   * Return a new Decimal whose value is this Decimal times `y`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   */
  P.times = P.mul = function (y) {
    var carry, e, i, k, r, rL, t, xdL, ydL,
      x = this,
      Ctor = x.constructor,
      xd = x.d,
      yd = (y = new Ctor(y)).d;

    y.s *= x.s;

     // If either is NaN, ±Infinity or ±0...
    if (!xd || !xd[0] || !yd || !yd[0]) {

      return new Ctor(!y.s || xd && !xd[0] && !yd || yd && !yd[0] && !xd

        // Return NaN if either is NaN.
        // Return NaN if x is ±0 and y is ±Infinity, or y is ±0 and x is ±Infinity.
        ? NaN

        // Return ±Infinity if either is ±Infinity.
        // Return ±0 if either is ±0.
        : !xd || !yd ? y.s / 0 : y.s * 0);
    }

    e = mathfloor(x.e / LOG_BASE) + mathfloor(y.e / LOG_BASE);
    xdL = xd.length;
    ydL = yd.length;

    // Ensure xd points to the longer array.
    if (xdL < ydL) {
      r = xd;
      xd = yd;
      yd = r;
      rL = xdL;
      xdL = ydL;
      ydL = rL;
    }

    // Initialise the result array with zeros.
    r = [];
    rL = xdL + ydL;
    for (i = rL; i--;) r.push(0);

    // Multiply!
    for (i = ydL; --i >= 0;) {
      carry = 0;
      for (k = xdL + i; k > i;) {
        t = r[k] + yd[i] * xd[k - i - 1] + carry;
        r[k--] = t % BASE | 0;
        carry = t / BASE | 0;
      }

      r[k] = (r[k] + carry) % BASE | 0;
    }

    // Remove trailing zeros.
    for (; !r[--rL];) r.pop();

    if (carry) ++e;
    else r.shift();

    y.d = r;
    y.e = getBase10Exponent(r, e);

    return external ? finalise(y, Ctor.precision, Ctor.rounding) : y;
  };


  /*
   * Return a string representing the value of this Decimal in base 2, round to `sd` significant
   * digits using rounding mode `rm`.
   *
   * If the optional `sd` argument is present then return binary exponential notation.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toBinary = function (sd, rm) {
    return toStringBinary(this, 2, sd, rm);
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a maximum of `dp`
   * decimal places using rounding mode `rm` or `rounding` if `rm` is omitted.
   *
   * If `dp` is omitted, return a new Decimal whose value is the value of this Decimal.
   *
   * [dp] {number} Decimal places. Integer, 0 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toDecimalPlaces = P.toDP = function (dp, rm) {
    var x = this,
      Ctor = x.constructor;

    x = new Ctor(x);
    if (dp === void 0) return x;

    checkInt32(dp, 0, MAX_DIGITS);

    if (rm === void 0) rm = Ctor.rounding;
    else checkInt32(rm, 0, 8);

    return finalise(x, dp + x.e + 1, rm);
  };


  /*
   * Return a string representing the value of this Decimal in exponential notation rounded to
   * `dp` fixed decimal places using rounding mode `rounding`.
   *
   * [dp] {number} Decimal places. Integer, 0 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toExponential = function (dp, rm) {
    var str,
      x = this,
      Ctor = x.constructor;

    if (dp === void 0) {
      str = finiteToString(x, true);
    } else {
      checkInt32(dp, 0, MAX_DIGITS);

      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);

      x = finalise(new Ctor(x), dp + 1, rm);
      str = finiteToString(x, true, dp + 1);
    }

    return x.isNeg() && !x.isZero() ? '-' + str : str;
  };


  /*
   * Return a string representing the value of this Decimal in normal (fixed-point) notation to
   * `dp` fixed decimal places and rounded using rounding mode `rm` or `rounding` if `rm` is
   * omitted.
   *
   * As with JavaScript numbers, (-0).toFixed(0) is '0', but e.g. (-0.00001).toFixed(0) is '-0'.
   *
   * [dp] {number} Decimal places. Integer, 0 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   * (-0).toFixed(0) is '0', but (-0.1).toFixed(0) is '-0'.
   * (-0).toFixed(1) is '0.0', but (-0.01).toFixed(1) is '-0.0'.
   * (-0).toFixed(3) is '0.000'.
   * (-0.5).toFixed(0) is '-0'.
   *
   */
  P.toFixed = function (dp, rm) {
    var str, y,
      x = this,
      Ctor = x.constructor;

    if (dp === void 0) {
      str = finiteToString(x);
    } else {
      checkInt32(dp, 0, MAX_DIGITS);

      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);

      y = finalise(new Ctor(x), dp + x.e + 1, rm);
      str = finiteToString(y, false, dp + y.e + 1);
    }

    // To determine whether to add the minus sign look at the value before it was rounded,
    // i.e. look at `x` rather than `y`.
    return x.isNeg() && !x.isZero() ? '-' + str : str;
  };


  /*
   * Return an array representing the value of this Decimal as a simple fraction with an integer
   * numerator and an integer denominator.
   *
   * The denominator will be a positive non-zero value less than or equal to the specified maximum
   * denominator. If a maximum denominator is not specified, the denominator will be the lowest
   * value necessary to represent the number exactly.
   *
   * [maxD] {number|string|Decimal} Maximum denominator. Integer >= 1 and < Infinity.
   *
   */
  P.toFraction = function (maxD) {
    var d, d0, d1, d2, e, k, n, n0, n1, pr, q, r,
      x = this,
      xd = x.d,
      Ctor = x.constructor;

    if (!xd) return new Ctor(x);

    n1 = d0 = new Ctor(1);
    d1 = n0 = new Ctor(0);

    d = new Ctor(d1);
    e = d.e = getPrecision(xd) - x.e - 1;
    k = e % LOG_BASE;
    d.d[0] = mathpow(10, k < 0 ? LOG_BASE + k : k);

    if (maxD == null) {

      // d is 10**e, the minimum max-denominator needed.
      maxD = e > 0 ? d : n1;
    } else {
      n = new Ctor(maxD);
      if (!n.isInt() || n.lt(n1)) throw Error(invalidArgument + n);
      maxD = n.gt(d) ? (e > 0 ? d : n1) : n;
    }

    external = false;
    n = new Ctor(digitsToString(xd));
    pr = Ctor.precision;
    Ctor.precision = e = xd.length * LOG_BASE * 2;

    for (;;)  {
      q = divide(n, d, 0, 1, 1);
      d2 = d0.plus(q.times(d1));
      if (d2.cmp(maxD) == 1) break;
      d0 = d1;
      d1 = d2;
      d2 = n1;
      n1 = n0.plus(q.times(d2));
      n0 = d2;
      d2 = d;
      d = n.minus(q.times(d2));
      n = d2;
    }

    d2 = divide(maxD.minus(d0), d1, 0, 1, 1);
    n0 = n0.plus(d2.times(n1));
    d0 = d0.plus(d2.times(d1));
    n0.s = n1.s = x.s;

    // Determine which fraction is closer to x, n0/d0 or n1/d1?
    r = divide(n1, d1, e, 1).minus(x).abs().cmp(divide(n0, d0, e, 1).minus(x).abs()) < 1
        ? [n1, d1] : [n0, d0];

    Ctor.precision = pr;
    external = true;

    return r;
  };


  /*
   * Return a string representing the value of this Decimal in base 16, round to `sd` significant
   * digits using rounding mode `rm`.
   *
   * If the optional `sd` argument is present then return binary exponential notation.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toHexadecimal = P.toHex = function (sd, rm) {
    return toStringBinary(this, 16, sd, rm);
  };



  /*
   * Returns a new Decimal whose value is the nearest multiple of the magnitude of `y` to the value
   * of this Decimal.
   *
   * If the value of this Decimal is equidistant from two multiples of `y`, the rounding mode `rm`,
   * or `Decimal.rounding` if `rm` is omitted, determines the direction of the nearest multiple.
   *
   * In the context of this method, rounding mode 4 (ROUND_HALF_UP) is the same as rounding mode 0
   * (ROUND_UP), and so on.
   *
   * The return value will always have the same sign as this Decimal, unless either this Decimal
   * or `y` is NaN, in which case the return value will be also be NaN.
   *
   * The return value is not affected by the value of `precision`.
   *
   * y {number|string|Decimal} The magnitude to round to a multiple of.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   * 'toNearest() rounding mode not an integer: {rm}'
   * 'toNearest() rounding mode out of range: {rm}'
   *
   */
  P.toNearest = function (y, rm) {
    var x = this,
      Ctor = x.constructor;

    x = new Ctor(x);

    if (y == null) {

      // If x is not finite, return x.
      if (!x.d) return x;

      y = new Ctor(1);
      rm = Ctor.rounding;
    } else {
      y = new Ctor(y);
      if (rm !== void 0) checkInt32(rm, 0, 8);

      // If x is not finite, return x if y is not NaN, else NaN.
      if (!x.d) return y.s ? x : y;

      // If y is not finite, return Infinity with the sign of x if y is Infinity, else NaN.
      if (!y.d) {
        if (y.s) y.s = x.s;
        return y;
      }
    }

    // If y is not zero, calculate the nearest multiple of y to x.
    if (y.d[0]) {
      external = false;
      if (rm < 4) rm = [4, 5, 7, 8][rm];
      x = divide(x, y, 0, rm, 1).times(y);
      external = true;
      finalise(x);

    // If y is zero, return zero with the sign of x.
    } else {
      y.s = x.s;
      x = y;
    }

    return x;
  };


  /*
   * Return the value of this Decimal converted to a number primitive.
   * Zero keeps its sign.
   *
   */
  P.toNumber = function () {
    return +this;
  };


  /*
   * Return a string representing the value of this Decimal in base 8, round to `sd` significant
   * digits using rounding mode `rm`.
   *
   * If the optional `sd` argument is present then return binary exponential notation.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toOctal = function (sd, rm) {
    return toStringBinary(this, 8, sd, rm);
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal raised to the power `y`, rounded
   * to `precision` significant digits using rounding mode `rounding`.
   *
   * ECMAScript compliant.
   *
   *   pow(x, NaN)                           = NaN
   *   pow(x, ±0)                            = 1

   *   pow(NaN, non-zero)                    = NaN
   *   pow(abs(x) > 1, +Infinity)            = +Infinity
   *   pow(abs(x) > 1, -Infinity)            = +0
   *   pow(abs(x) == 1, ±Infinity)           = NaN
   *   pow(abs(x) < 1, +Infinity)            = +0
   *   pow(abs(x) < 1, -Infinity)            = +Infinity
   *   pow(+Infinity, y > 0)                 = +Infinity
   *   pow(+Infinity, y < 0)                 = +0
   *   pow(-Infinity, odd integer > 0)       = -Infinity
   *   pow(-Infinity, even integer > 0)      = +Infinity
   *   pow(-Infinity, odd integer < 0)       = -0
   *   pow(-Infinity, even integer < 0)      = +0
   *   pow(+0, y > 0)                        = +0
   *   pow(+0, y < 0)                        = +Infinity
   *   pow(-0, odd integer > 0)              = -0
   *   pow(-0, even integer > 0)             = +0
   *   pow(-0, odd integer < 0)              = -Infinity
   *   pow(-0, even integer < 0)             = +Infinity
   *   pow(finite x < 0, finite non-integer) = NaN
   *
   * For non-integer or very large exponents pow(x, y) is calculated using
   *
   *   x^y = exp(y*ln(x))
   *
   * Assuming the first 15 rounding digits are each equally likely to be any digit 0-9, the
   * probability of an incorrectly rounded result
   * P([49]9{14} | [50]0{14}) = 2 * 0.2 * 10^-14 = 4e-15 = 1/2.5e+14
   * i.e. 1 in 250,000,000,000,000
   *
   * If a result is incorrectly rounded the maximum error will be 1 ulp (unit in last place).
   *
   * y {number|string|Decimal} The power to which to raise this Decimal.
   *
   */
  P.toPower = P.pow = function (y) {
    var e, k, pr, r, rm, s,
      x = this,
      Ctor = x.constructor,
      yn = +(y = new Ctor(y));

    // Either ±Infinity, NaN or ±0?
    if (!x.d || !y.d || !x.d[0] || !y.d[0]) return new Ctor(mathpow(+x, yn));

    x = new Ctor(x);

    if (x.eq(1)) return x;

    pr = Ctor.precision;
    rm = Ctor.rounding;

    if (y.eq(1)) return finalise(x, pr, rm);

    // y exponent
    e = mathfloor(y.e / LOG_BASE);

    // If y is a small integer use the 'exponentiation by squaring' algorithm.
    if (e >= y.d.length - 1 && (k = yn < 0 ? -yn : yn) <= MAX_SAFE_INTEGER) {
      r = intPow(Ctor, x, k, pr);
      return y.s < 0 ? new Ctor(1).div(r) : finalise(r, pr, rm);
    }

    s = x.s;

    // if x is negative
    if (s < 0) {

      // if y is not an integer
      if (e < y.d.length - 1) return new Ctor(NaN);

      // Result is positive if x is negative and the last digit of integer y is even.
      if ((y.d[e] & 1) == 0) s = 1;

      // if x.eq(-1)
      if (x.e == 0 && x.d[0] == 1 && x.d.length == 1) {
        x.s = s;
        return x;
      }
    }

    // Estimate result exponent.
    // x^y = 10^e,  where e = y * log10(x)
    // log10(x) = log10(x_significand) + x_exponent
    // log10(x_significand) = ln(x_significand) / ln(10)
    k = mathpow(+x, yn);
    e = k == 0 || !isFinite(k)
      ? mathfloor(yn * (Math.log('0.' + digitsToString(x.d)) / Math.LN10 + x.e + 1))
      : new Ctor(k + '').e;

    // Exponent estimate may be incorrect e.g. x: 0.999999999999999999, y: 2.29, e: 0, r.e: -1.

    // Overflow/underflow?
    if (e > Ctor.maxE + 1 || e < Ctor.minE - 1) return new Ctor(e > 0 ? s / 0 : 0);

    external = false;
    Ctor.rounding = x.s = 1;

    // Estimate the extra guard digits needed to ensure five correct rounding digits from
    // naturalLogarithm(x). Example of failure without these extra digits (precision: 10):
    // new Decimal(2.32456).pow('2087987436534566.46411')
    // should be 1.162377823e+764914905173815, but is 1.162355823e+764914905173815
    k = Math.min(12, (e + '').length);

    // r = x^y = exp(y*ln(x))
    r = naturalExponential(y.times(naturalLogarithm(x, pr + k)), pr);

    // r may be Infinity, e.g. (0.9999999999999999).pow(-1e+40)
    if (r.d) {

      // Truncate to the required precision plus five rounding digits.
      r = finalise(r, pr + 5, 1);

      // If the rounding digits are [49]9999 or [50]0000 increase the precision by 10 and recalculate
      // the result.
      if (checkRoundingDigits(r.d, pr, rm)) {
        e = pr + 10;

        // Truncate to the increased precision plus five rounding digits.
        r = finalise(naturalExponential(y.times(naturalLogarithm(x, e + k)), e), e + 5, 1);

        // Check for 14 nines from the 2nd rounding digit (the first rounding digit may be 4 or 9).
        if (+digitsToString(r.d).slice(pr + 1, pr + 15) + 1 == 1e14) {
          r = finalise(r, pr + 1, 0);
        }
      }
    }

    r.s = s;
    external = true;
    Ctor.rounding = rm;

    return finalise(r, pr, rm);
  };


  /*
   * Return a string representing the value of this Decimal rounded to `sd` significant digits
   * using rounding mode `rounding`.
   *
   * Return exponential notation if `sd` is less than the number of digits necessary to represent
   * the integer part of the value in normal notation.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   */
  P.toPrecision = function (sd, rm) {
    var str,
      x = this,
      Ctor = x.constructor;

    if (sd === void 0) {
      str = finiteToString(x, x.e <= Ctor.toExpNeg || x.e >= Ctor.toExpPos);
    } else {
      checkInt32(sd, 1, MAX_DIGITS);

      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);

      x = finalise(new Ctor(x), sd, rm);
      str = finiteToString(x, sd <= x.e || x.e <= Ctor.toExpNeg, sd);
    }

    return x.isNeg() && !x.isZero() ? '-' + str : str;
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal rounded to a maximum of `sd`
   * significant digits using rounding mode `rm`, or to `precision` and `rounding` respectively if
   * omitted.
   *
   * [sd] {number} Significant digits. Integer, 1 to MAX_DIGITS inclusive.
   * [rm] {number} Rounding mode. Integer, 0 to 8 inclusive.
   *
   * 'toSD() digits out of range: {sd}'
   * 'toSD() digits not an integer: {sd}'
   * 'toSD() rounding mode not an integer: {rm}'
   * 'toSD() rounding mode out of range: {rm}'
   *
   */
  P.toSignificantDigits = P.toSD = function (sd, rm) {
    var x = this,
      Ctor = x.constructor;

    if (sd === void 0) {
      sd = Ctor.precision;
      rm = Ctor.rounding;
    } else {
      checkInt32(sd, 1, MAX_DIGITS);

      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);
    }

    return finalise(new Ctor(x), sd, rm);
  };


  /*
   * Return a string representing the value of this Decimal.
   *
   * Return exponential notation if this Decimal has a positive exponent equal to or greater than
   * `toExpPos`, or a negative exponent equal to or less than `toExpNeg`.
   *
   */
  P.toString = function () {
    var x = this,
      Ctor = x.constructor,
      str = finiteToString(x, x.e <= Ctor.toExpNeg || x.e >= Ctor.toExpPos);

    return x.isNeg() && !x.isZero() ? '-' + str : str;
  };


  /*
   * Return a new Decimal whose value is the value of this Decimal truncated to a whole number.
   *
   */
  P.truncated = P.trunc = function () {
    return finalise(new this.constructor(this), this.e + 1, 1);
  };


  /*
   * Return a string representing the value of this Decimal.
   * Unlike `toString`, negative zero will include the minus sign.
   *
   */
  P.valueOf = P.toJSON = function () {
    var x = this,
      Ctor = x.constructor,
      str = finiteToString(x, x.e <= Ctor.toExpNeg || x.e >= Ctor.toExpPos);

    return x.isNeg() ? '-' + str : str;
  };


  /*
  // Add aliases to match BigDecimal method names.
  // P.add = P.plus;
  P.subtract = P.minus;
  P.multiply = P.times;
  P.divide = P.div;
  P.remainder = P.mod;
  P.compareTo = P.cmp;
  P.negate = P.neg;
   */


  // Helper functions for Decimal.prototype (P) and/or Decimal methods, and their callers.


  /*
   *  digitsToString           P.cubeRoot, P.logarithm, P.squareRoot, P.toFraction, P.toPower,
   *                           finiteToString, naturalExponential, naturalLogarithm
   *  checkInt32               P.toDecimalPlaces, P.toExponential, P.toFixed, P.toNearest,
   *                           P.toPrecision, P.toSignificantDigits, toStringBinary, random
   *  checkRoundingDigits      P.logarithm, P.toPower, naturalExponential, naturalLogarithm
   *  convertBase              toStringBinary, parseOther
   *  cos                      P.cos
   *  divide                   P.atanh, P.cubeRoot, P.dividedBy, P.dividedToIntegerBy,
   *                           P.logarithm, P.modulo, P.squareRoot, P.tan, P.tanh, P.toFraction,
   *                           P.toNearest, toStringBinary, naturalExponential, naturalLogarithm,
   *                           taylorSeries, atan2, parseOther
   *  finalise                 P.absoluteValue, P.atan, P.atanh, P.ceil, P.cos, P.cosh,
   *                           P.cubeRoot, P.dividedToIntegerBy, P.floor, P.logarithm, P.minus,
   *                           P.modulo, P.negated, P.plus, P.round, P.sin, P.sinh, P.squareRoot,
   *                           P.tan, P.times, P.toDecimalPlaces, P.toExponential, P.toFixed,
   *                           P.toNearest, P.toPower, P.toPrecision, P.toSignificantDigits,
   *                           P.truncated, divide, getLn10, getPi, naturalExponential,
   *                           naturalLogarithm, ceil, floor, round, trunc
   *  finiteToString           P.toExponential, P.toFixed, P.toPrecision, P.toString, P.valueOf,
   *                           toStringBinary
   *  getBase10Exponent        P.minus, P.plus, P.times, parseOther
   *  getLn10                  P.logarithm, naturalLogarithm
   *  getPi                    P.acos, P.asin, P.atan, toLessThanHalfPi, atan2
   *  getPrecision             P.precision, P.toFraction
   *  getZeroString            digitsToString, finiteToString
   *  intPow                   P.toPower, parseOther
   *  isOdd                    toLessThanHalfPi
   *  maxOrMin                 max, min
   *  naturalExponential       P.naturalExponential, P.toPower
   *  naturalLogarithm         P.acosh, P.asinh, P.atanh, P.logarithm, P.naturalLogarithm,
   *                           P.toPower, naturalExponential
   *  nonFiniteToString        finiteToString, toStringBinary
   *  parseDecimal             Decimal
   *  parseOther               Decimal
   *  sin                      P.sin
   *  taylorSeries             P.cosh, P.sinh, cos, sin
   *  toLessThanHalfPi         P.cos, P.sin
   *  toStringBinary           P.toBinary, P.toHexadecimal, P.toOctal
   *  truncate                 intPow
   *
   *  Throws:                  P.logarithm, P.precision, P.toFraction, checkInt32, getLn10, getPi,
   *                           naturalLogarithm, config, parseOther, random, Decimal
   */


  function digitsToString(d) {
    var i, k, ws,
      indexOfLastWord = d.length - 1,
      str = '',
      w = d[0];

    if (indexOfLastWord > 0) {
      str += w;
      for (i = 1; i < indexOfLastWord; i++) {
        ws = d[i] + '';
        k = LOG_BASE - ws.length;
        if (k) str += getZeroString(k);
        str += ws;
      }

      w = d[i];
      ws = w + '';
      k = LOG_BASE - ws.length;
      if (k) str += getZeroString(k);
    } else if (w === 0) {
      return '0';
    }

    // Remove trailing zeros of last w.
    for (; w % 10 === 0;) w /= 10;

    return str + w;
  }


  function checkInt32(i, min, max) {
    if (i !== ~~i || i < min || i > max) {
      throw Error(invalidArgument + i);
    }
  }


  /*
   * Check 5 rounding digits if `repeating` is null, 4 otherwise.
   * `repeating == null` if caller is `log` or `pow`,
   * `repeating != null` if caller is `naturalLogarithm` or `naturalExponential`.
   */
  function checkRoundingDigits(d, i, rm, repeating) {
    var di, k, r, rd;

    // Get the length of the first word of the array d.
    for (k = d[0]; k >= 10; k /= 10) --i;

    // Is the rounding digit in the first word of d?
    if (--i < 0) {
      i += LOG_BASE;
      di = 0;
    } else {
      di = Math.ceil((i + 1) / LOG_BASE);
      i %= LOG_BASE;
    }

    // i is the index (0 - 6) of the rounding digit.
    // E.g. if within the word 3487563 the first rounding digit is 5,
    // then i = 4, k = 1000, rd = 3487563 % 1000 = 563
    k = mathpow(10, LOG_BASE - i);
    rd = d[di] % k | 0;

    if (repeating == null) {
      if (i < 3) {
        if (i == 0) rd = rd / 100 | 0;
        else if (i == 1) rd = rd / 10 | 0;
        r = rm < 4 && rd == 99999 || rm > 3 && rd == 49999 || rd == 50000 || rd == 0;
      } else {
        r = (rm < 4 && rd + 1 == k || rm > 3 && rd + 1 == k / 2) &&
          (d[di + 1] / k / 100 | 0) == mathpow(10, i - 2) - 1 ||
            (rd == k / 2 || rd == 0) && (d[di + 1] / k / 100 | 0) == 0;
      }
    } else {
      if (i < 4) {
        if (i == 0) rd = rd / 1000 | 0;
        else if (i == 1) rd = rd / 100 | 0;
        else if (i == 2) rd = rd / 10 | 0;
        r = (repeating || rm < 4) && rd == 9999 || !repeating && rm > 3 && rd == 4999;
      } else {
        r = ((repeating || rm < 4) && rd + 1 == k ||
        (!repeating && rm > 3) && rd + 1 == k / 2) &&
          (d[di + 1] / k / 1000 | 0) == mathpow(10, i - 3) - 1;
      }
    }

    return r;
  }


  // Convert string of `baseIn` to an array of numbers of `baseOut`.
  // Eg. convertBase('255', 10, 16) returns [15, 15].
  // Eg. convertBase('ff', 16, 10) returns [2, 5, 5].
  function convertBase(str, baseIn, baseOut) {
    var j,
      arr = [0],
      arrL,
      i = 0,
      strL = str.length;

    for (; i < strL;) {
      for (arrL = arr.length; arrL--;) arr[arrL] *= baseIn;
      arr[0] += NUMERALS.indexOf(str.charAt(i++));
      for (j = 0; j < arr.length; j++) {
        if (arr[j] > baseOut - 1) {
          if (arr[j + 1] === void 0) arr[j + 1] = 0;
          arr[j + 1] += arr[j] / baseOut | 0;
          arr[j] %= baseOut;
        }
      }
    }

    return arr.reverse();
  }


  /*
   * cos(x) = 1 - x^2/2! + x^4/4! - ...
   * |x| < pi/2
   *
   */
  function cosine(Ctor, x) {
    var k, y,
      len = x.d.length;

    // Argument reduction: cos(4x) = 8*(cos^4(x) - cos^2(x)) + 1
    // i.e. cos(x) = 8*(cos^4(x/4) - cos^2(x/4)) + 1

    // Estimate the optimum number of times to use the argument reduction.
    if (len < 32) {
      k = Math.ceil(len / 3);
      y = Math.pow(4, -k).toString();
    } else {
      k = 16;
      y = '2.3283064365386962890625e-10';
    }

    Ctor.precision += k;

    x = taylorSeries(Ctor, 1, x.times(y), new Ctor(1));

    // Reverse argument reduction
    for (var i = k; i--;) {
      var cos2x = x.times(x);
      x = cos2x.times(cos2x).minus(cos2x).times(8).plus(1);
    }

    Ctor.precision -= k;

    return x;
  }


  /*
   * Perform division in the specified base.
   */
  var divide = (function () {

    // Assumes non-zero x and k, and hence non-zero result.
    function multiplyInteger(x, k, base) {
      var temp,
        carry = 0,
        i = x.length;

      for (x = x.slice(); i--;) {
        temp = x[i] * k + carry;
        x[i] = temp % base | 0;
        carry = temp / base | 0;
      }

      if (carry) x.unshift(carry);

      return x;
    }

    function compare(a, b, aL, bL) {
      var i, r;

      if (aL != bL) {
        r = aL > bL ? 1 : -1;
      } else {
        for (i = r = 0; i < aL; i++) {
          if (a[i] != b[i]) {
            r = a[i] > b[i] ? 1 : -1;
            break;
          }
        }
      }

      return r;
    }

    function subtract(a, b, aL, base) {
      var i = 0;

      // Subtract b from a.
      for (; aL--;) {
        a[aL] -= i;
        i = a[aL] < b[aL] ? 1 : 0;
        a[aL] = i * base + a[aL] - b[aL];
      }

      // Remove leading zeros.
      for (; !a[0] && a.length > 1;) a.shift();
    }

    return function (x, y, pr, rm, dp, base) {
      var cmp, e, i, k, logBase, more, prod, prodL, q, qd, rem, remL, rem0, sd, t, xi, xL, yd0,
        yL, yz,
        Ctor = x.constructor,
        sign = x.s == y.s ? 1 : -1,
        xd = x.d,
        yd = y.d;

      // Either NaN, Infinity or 0?
      if (!xd || !xd[0] || !yd || !yd[0]) {

        return new Ctor(// Return NaN if either NaN, or both Infinity or 0.
          !x.s || !y.s || (xd ? yd && xd[0] == yd[0] : !yd) ? NaN :

          // Return ±0 if x is 0 or y is ±Infinity, or return ±Infinity as y is 0.
          xd && xd[0] == 0 || !yd ? sign * 0 : sign / 0);
      }

      if (base) {
        logBase = 1;
        e = x.e - y.e;
      } else {
        base = BASE;
        logBase = LOG_BASE;
        e = mathfloor(x.e / logBase) - mathfloor(y.e / logBase);
      }

      yL = yd.length;
      xL = xd.length;
      q = new Ctor(sign);
      qd = q.d = [];

      // Result exponent may be one less than e.
      // The digit array of a Decimal from toStringBinary may have trailing zeros.
      for (i = 0; yd[i] == (xd[i] || 0); i++);

      if (yd[i] > (xd[i] || 0)) e--;

      if (pr == null) {
        sd = pr = Ctor.precision;
        rm = Ctor.rounding;
      } else if (dp) {
        sd = pr + (x.e - y.e) + 1;
      } else {
        sd = pr;
      }

      if (sd < 0) {
        qd.push(1);
        more = true;
      } else {

        // Convert precision in number of base 10 digits to base 1e7 digits.
        sd = sd / logBase + 2 | 0;
        i = 0;

        // divisor < 1e7
        if (yL == 1) {
          k = 0;
          yd = yd[0];
          sd++;

          // k is the carry.
          for (; (i < xL || k) && sd--; i++) {
            t = k * base + (xd[i] || 0);
            qd[i] = t / yd | 0;
            k = t % yd | 0;
          }

          more = k || i < xL;

        // divisor >= 1e7
        } else {

          // Normalise xd and yd so highest order digit of yd is >= base/2
          k = base / (yd[0] + 1) | 0;

          if (k > 1) {
            yd = multiplyInteger(yd, k, base);
            xd = multiplyInteger(xd, k, base);
            yL = yd.length;
            xL = xd.length;
          }

          xi = yL;
          rem = xd.slice(0, yL);
          remL = rem.length;

          // Add zeros to make remainder as long as divisor.
          for (; remL < yL;) rem[remL++] = 0;

          yz = yd.slice();
          yz.unshift(0);
          yd0 = yd[0];

          if (yd[1] >= base / 2) ++yd0;

          do {
            k = 0;

            // Compare divisor and remainder.
            cmp = compare(yd, rem, yL, remL);

            // If divisor < remainder.
            if (cmp < 0) {

              // Calculate trial digit, k.
              rem0 = rem[0];
              if (yL != remL) rem0 = rem0 * base + (rem[1] || 0);

              // k will be how many times the divisor goes into the current remainder.
              k = rem0 / yd0 | 0;

              //  Algorithm:
              //  1. product = divisor * trial digit (k)
              //  2. if product > remainder: product -= divisor, k--
              //  3. remainder -= product
              //  4. if product was < remainder at 2:
              //    5. compare new remainder and divisor
              //    6. If remainder > divisor: remainder -= divisor, k++

              if (k > 1) {
                if (k >= base) k = base - 1;

                // product = divisor * trial digit.
                prod = multiplyInteger(yd, k, base);
                prodL = prod.length;
                remL = rem.length;

                // Compare product and remainder.
                cmp = compare(prod, rem, prodL, remL);

                // product > remainder.
                if (cmp == 1) {
                  k--;

                  // Subtract divisor from product.
                  subtract(prod, yL < prodL ? yz : yd, prodL, base);
                }
              } else {

                // cmp is -1.
                // If k is 0, there is no need to compare yd and rem again below, so change cmp to 1
                // to avoid it. If k is 1 there is a need to compare yd and rem again below.
                if (k == 0) cmp = k = 1;
                prod = yd.slice();
              }

              prodL = prod.length;
              if (prodL < remL) prod.unshift(0);

              // Subtract product from remainder.
              subtract(rem, prod, remL, base);

              // If product was < previous remainder.
              if (cmp == -1) {
                remL = rem.length;

                // Compare divisor and new remainder.
                cmp = compare(yd, rem, yL, remL);

                // If divisor < new remainder, subtract divisor from remainder.
                if (cmp < 1) {
                  k++;

                  // Subtract divisor from remainder.
                  subtract(rem, yL < remL ? yz : yd, remL, base);
                }
              }

              remL = rem.length;
            } else if (cmp === 0) {
              k++;
              rem = [0];
            }    // if cmp === 1, k will be 0

            // Add the next digit, k, to the result array.
            qd[i++] = k;

            // Update the remainder.
            if (cmp && rem[0]) {
              rem[remL++] = xd[xi] || 0;
            } else {
              rem = [xd[xi]];
              remL = 1;
            }

          } while ((xi++ < xL || rem[0] !== void 0) && sd--);

          more = rem[0] !== void 0;
        }

        // Leading zero?
        if (!qd[0]) qd.shift();
      }

      // logBase is 1 when divide is being used for base conversion.
      if (logBase == 1) {
        q.e = e;
        inexact = more;
      } else {

        // To calculate q.e, first get the number of digits of qd[0].
        for (i = 1, k = qd[0]; k >= 10; k /= 10) i++;
        q.e = i + e * logBase - 1;

        finalise(q, dp ? pr + q.e + 1 : pr, rm, more);
      }

      return q;
    };
  })();


  /*
   * Round `x` to `sd` significant digits using rounding mode `rm`.
   * Check for over/under-flow.
   */
   function finalise(x, sd, rm, isTruncated) {
    var digits, i, j, k, rd, roundUp, w, xd, xdi,
      Ctor = x.constructor;

    // Don't round if sd is null or undefined.
    out: if (sd != null) {
      xd = x.d;

      // Infinity/NaN.
      if (!xd) return x;

      // rd: the rounding digit, i.e. the digit after the digit that may be rounded up.
      // w: the word of xd containing rd, a base 1e7 number.
      // xdi: the index of w within xd.
      // digits: the number of digits of w.
      // i: what would be the index of rd within w if all the numbers were 7 digits long (i.e. if
      // they had leading zeros)
      // j: if > 0, the actual index of rd within w (if < 0, rd is a leading zero).

      // Get the length of the first word of the digits array xd.
      for (digits = 1, k = xd[0]; k >= 10; k /= 10) digits++;
      i = sd - digits;

      // Is the rounding digit in the first word of xd?
      if (i < 0) {
        i += LOG_BASE;
        j = sd;
        w = xd[xdi = 0];

        // Get the rounding digit at index j of w.
        rd = w / mathpow(10, digits - j - 1) % 10 | 0;
      } else {
        xdi = Math.ceil((i + 1) / LOG_BASE);
        k = xd.length;
        if (xdi >= k) {
          if (isTruncated) {

            // Needed by `naturalExponential`, `naturalLogarithm` and `squareRoot`.
            for (; k++ <= xdi;) xd.push(0);
            w = rd = 0;
            digits = 1;
            i %= LOG_BASE;
            j = i - LOG_BASE + 1;
          } else {
            break out;
          }
        } else {
          w = k = xd[xdi];

          // Get the number of digits of w.
          for (digits = 1; k >= 10; k /= 10) digits++;

          // Get the index of rd within w.
          i %= LOG_BASE;

          // Get the index of rd within w, adjusted for leading zeros.
          // The number of leading zeros of w is given by LOG_BASE - digits.
          j = i - LOG_BASE + digits;

          // Get the rounding digit at index j of w.
          rd = j < 0 ? 0 : w / mathpow(10, digits - j - 1) % 10 | 0;
        }
      }

      // Are there any non-zero digits after the rounding digit?
      isTruncated = isTruncated || sd < 0 ||
        xd[xdi + 1] !== void 0 || (j < 0 ? w : w % mathpow(10, digits - j - 1));

      // The expression `w % mathpow(10, digits - j - 1)` returns all the digits of w to the right
      // of the digit at (left-to-right) index j, e.g. if w is 908714 and j is 2, the expression
      // will give 714.

      roundUp = rm < 4
        ? (rd || isTruncated) && (rm == 0 || rm == (x.s < 0 ? 3 : 2))
        : rd > 5 || rd == 5 && (rm == 4 || isTruncated || rm == 6 &&

          // Check whether the digit to the left of the rounding digit is odd.
          ((i > 0 ? j > 0 ? w / mathpow(10, digits - j) : 0 : xd[xdi - 1]) % 10) & 1 ||
            rm == (x.s < 0 ? 8 : 7));

      if (sd < 1 || !xd[0]) {
        xd.length = 0;
        if (roundUp) {

          // Convert sd to decimal places.
          sd -= x.e + 1;

          // 1, 0.1, 0.01, 0.001, 0.0001 etc.
          xd[0] = mathpow(10, (LOG_BASE - sd % LOG_BASE) % LOG_BASE);
          x.e = -sd || 0;
        } else {

          // Zero.
          xd[0] = x.e = 0;
        }

        return x;
      }

      // Remove excess digits.
      if (i == 0) {
        xd.length = xdi;
        k = 1;
        xdi--;
      } else {
        xd.length = xdi + 1;
        k = mathpow(10, LOG_BASE - i);

        // E.g. 56700 becomes 56000 if 7 is the rounding digit.
        // j > 0 means i > number of leading zeros of w.
        xd[xdi] = j > 0 ? (w / mathpow(10, digits - j) % mathpow(10, j) | 0) * k : 0;
      }

      if (roundUp) {
        for (;;) {

          // Is the digit to be rounded up in the first word of xd?
          if (xdi == 0) {

            // i will be the length of xd[0] before k is added.
            for (i = 1, j = xd[0]; j >= 10; j /= 10) i++;
            j = xd[0] += k;
            for (k = 1; j >= 10; j /= 10) k++;

            // if i != k the length has increased.
            if (i != k) {
              x.e++;
              if (xd[0] == BASE) xd[0] = 1;
            }

            break;
          } else {
            xd[xdi] += k;
            if (xd[xdi] != BASE) break;
            xd[xdi--] = 0;
            k = 1;
          }
        }
      }

      // Remove trailing zeros.
      for (i = xd.length; xd[--i] === 0;) xd.pop();
    }

    if (external) {

      // Overflow?
      if (x.e > Ctor.maxE) {

        // Infinity.
        x.d = null;
        x.e = NaN;

      // Underflow?
      } else if (x.e < Ctor.minE) {

        // Zero.
        x.e = 0;
        x.d = [0];
        // Ctor.underflow = true;
      } // else Ctor.underflow = false;
    }

    return x;
  }


  function finiteToString(x, isExp, sd) {
    if (!x.isFinite()) return nonFiniteToString(x);
    var k,
      e = x.e,
      str = digitsToString(x.d),
      len = str.length;

    if (isExp) {
      if (sd && (k = sd - len) > 0) {
        str = str.charAt(0) + '.' + str.slice(1) + getZeroString(k);
      } else if (len > 1) {
        str = str.charAt(0) + '.' + str.slice(1);
      }

      str = str + (x.e < 0 ? 'e' : 'e+') + x.e;
    } else if (e < 0) {
      str = '0.' + getZeroString(-e - 1) + str;
      if (sd && (k = sd - len) > 0) str += getZeroString(k);
    } else if (e >= len) {
      str += getZeroString(e + 1 - len);
      if (sd && (k = sd - e - 1) > 0) str = str + '.' + getZeroString(k);
    } else {
      if ((k = e + 1) < len) str = str.slice(0, k) + '.' + str.slice(k);
      if (sd && (k = sd - len) > 0) {
        if (e + 1 === len) str += '.';
        str += getZeroString(k);
      }
    }

    return str;
  }


  // Calculate the base 10 exponent from the base 1e7 exponent.
  function getBase10Exponent(digits, e) {
    var w = digits[0];

    // Add the number of digits of the first word of the digits array.
    for ( e *= LOG_BASE; w >= 10; w /= 10) e++;
    return e;
  }


  function getLn10(Ctor, sd, pr) {
    if (sd > LN10_PRECISION) {

      // Reset global state in case the exception is caught.
      external = true;
      if (pr) Ctor.precision = pr;
      throw Error(precisionLimitExceeded);
    }
    return finalise(new Ctor(LN10), sd, 1, true);
  }


  function getPi(Ctor, sd, rm) {
    if (sd > PI_PRECISION) throw Error(precisionLimitExceeded);
    return finalise(new Ctor(PI), sd, rm, true);
  }


  function getPrecision(digits) {
    var w = digits.length - 1,
      len = w * LOG_BASE + 1;

    w = digits[w];

    // If non-zero...
    if (w) {

      // Subtract the number of trailing zeros of the last word.
      for (; w % 10 == 0; w /= 10) len--;

      // Add the number of digits of the first word.
      for (w = digits[0]; w >= 10; w /= 10) len++;
    }

    return len;
  }


  function getZeroString(k) {
    var zs = '';
    for (; k--;) zs += '0';
    return zs;
  }


  /*
   * Return a new Decimal whose value is the value of Decimal `x` to the power `n`, where `n` is an
   * integer of type number.
   *
   * Implements 'exponentiation by squaring'. Called by `pow` and `parseOther`.
   *
   */
  function intPow(Ctor, x, n, pr) {
    var isTruncated,
      r = new Ctor(1),

      // Max n of 9007199254740991 takes 53 loop iterations.
      // Maximum digits array length; leaves [28, 34] guard digits.
      k = Math.ceil(pr / LOG_BASE + 4);

    external = false;

    for (;;) {
      if (n % 2) {
        r = r.times(x);
        if (truncate(r.d, k)) isTruncated = true;
      }

      n = mathfloor(n / 2);
      if (n === 0) {

        // To ensure correct rounding when r.d is truncated, increment the last word if it is zero.
        n = r.d.length - 1;
        if (isTruncated && r.d[n] === 0) ++r.d[n];
        break;
      }

      x = x.times(x);
      truncate(x.d, k);
    }

    external = true;

    return r;
  }


  function isOdd(n) {
    return n.d[n.d.length - 1] & 1;
  }


  /*
   * Handle `max` and `min`. `ltgt` is 'lt' or 'gt'.
   */
  function maxOrMin(Ctor, args, ltgt) {
    var y,
      x = new Ctor(args[0]),
      i = 0;

    for (; ++i < args.length;) {
      y = new Ctor(args[i]);
      if (!y.s) {
        x = y;
        break;
      } else if (x[ltgt](y)) {
        x = y;
      }
    }

    return x;
  }


  /*
   * Return a new Decimal whose value is the natural exponential of `x` rounded to `sd` significant
   * digits.
   *
   * Taylor/Maclaurin series.
   *
   * exp(x) = x^0/0! + x^1/1! + x^2/2! + x^3/3! + ...
   *
   * Argument reduction:
   *   Repeat x = x / 32, k += 5, until |x| < 0.1
   *   exp(x) = exp(x / 2^k)^(2^k)
   *
   * Previously, the argument was initially reduced by
   * exp(x) = exp(r) * 10^k  where r = x - k * ln10, k = floor(x / ln10)
   * to first put r in the range [0, ln10], before dividing by 32 until |x| < 0.1, but this was
   * found to be slower than just dividing repeatedly by 32 as above.
   *
   * Max integer argument: exp('20723265836946413') = 6.3e+9000000000000000
   * Min integer argument: exp('-20723265836946411') = 1.2e-9000000000000000
   * (Math object integer min/max: Math.exp(709) = 8.2e+307, Math.exp(-745) = 5e-324)
   *
   *  exp(Infinity)  = Infinity
   *  exp(-Infinity) = 0
   *  exp(NaN)       = NaN
   *  exp(±0)        = 1
   *
   *  exp(x) is non-terminating for any finite, non-zero x.
   *
   *  The result will always be correctly rounded.
   *
   */
  function naturalExponential(x, sd) {
    var denominator, guard, j, pow, sum, t, wpr,
      rep = 0,
      i = 0,
      k = 0,
      Ctor = x.constructor,
      rm = Ctor.rounding,
      pr = Ctor.precision;

    // 0/NaN/Infinity?
    if (!x.d || !x.d[0] || x.e > 17) {

      return new Ctor(x.d
        ? !x.d[0] ? 1 : x.s < 0 ? 0 : 1 / 0
        : x.s ? x.s < 0 ? 0 : x : 0 / 0);
    }

    if (sd == null) {
      external = false;
      wpr = pr;
    } else {
      wpr = sd;
    }

    t = new Ctor(0.03125);

    // while abs(x) >= 0.1
    while (x.e > -2) {

      // x = x / 2^5
      x = x.times(t);
      k += 5;
    }

    // Use 2 * log10(2^k) + 5 (empirically derived) to estimate the increase in precision
    // necessary to ensure the first 4 rounding digits are correct.
    guard = Math.log(mathpow(2, k)) / Math.LN10 * 2 + 5 | 0;
    wpr += guard;
    denominator = pow = sum = new Ctor(1);
    Ctor.precision = wpr;

    for (;;) {
      pow = finalise(pow.times(x), wpr, 1);
      denominator = denominator.times(++i);
      t = sum.plus(divide(pow, denominator, wpr, 1));

      if (digitsToString(t.d).slice(0, wpr) === digitsToString(sum.d).slice(0, wpr)) {
        j = k;
        while (j--) sum = finalise(sum.times(sum), wpr, 1);

        // Check to see if the first 4 rounding digits are [49]999.
        // If so, repeat the summation with a higher precision, otherwise
        // e.g. with precision: 18, rounding: 1
        // exp(18.404272462595034083567793919843761) = 98372560.1229999999 (should be 98372560.123)
        // `wpr - guard` is the index of first rounding digit.
        if (sd == null) {

          if (rep < 3 && checkRoundingDigits(sum.d, wpr - guard, rm, rep)) {
            Ctor.precision = wpr += 10;
            denominator = pow = t = new Ctor(1);
            i = 0;
            rep++;
          } else {
            return finalise(sum, Ctor.precision = pr, rm, external = true);
          }
        } else {
          Ctor.precision = pr;
          return sum;
        }
      }

      sum = t;
    }
  }


  /*
   * Return a new Decimal whose value is the natural logarithm of `x` rounded to `sd` significant
   * digits.
   *
   *  ln(-n)        = NaN
   *  ln(0)         = -Infinity
   *  ln(-0)        = -Infinity
   *  ln(1)         = 0
   *  ln(Infinity)  = Infinity
   *  ln(-Infinity) = NaN
   *  ln(NaN)       = NaN
   *
   *  ln(n) (n != 1) is non-terminating.
   *
   */
  function naturalLogarithm(y, sd) {
    var c, c0, denominator, e, numerator, rep, sum, t, wpr, x1, x2,
      n = 1,
      guard = 10,
      x = y,
      xd = x.d,
      Ctor = x.constructor,
      rm = Ctor.rounding,
      pr = Ctor.precision;

    // Is x negative or Infinity, NaN, 0 or 1?
    if (x.s < 0 || !xd || !xd[0] || !x.e && xd[0] == 1 && xd.length == 1) {
      return new Ctor(xd && !xd[0] ? -1 / 0 : x.s != 1 ? NaN : xd ? 0 : x);
    }

    if (sd == null) {
      external = false;
      wpr = pr;
    } else {
      wpr = sd;
    }

    Ctor.precision = wpr += guard;
    c = digitsToString(xd);
    c0 = c.charAt(0);

    if (Math.abs(e = x.e) < 1.5e15) {

      // Argument reduction.
      // The series converges faster the closer the argument is to 1, so using
      // ln(a^b) = b * ln(a),   ln(a) = ln(a^b) / b
      // multiply the argument by itself until the leading digits of the significand are 7, 8, 9,
      // 10, 11, 12 or 13, recording the number of multiplications so the sum of the series can
      // later be divided by this number, then separate out the power of 10 using
      // ln(a*10^b) = ln(a) + b*ln(10).

      // max n is 21 (gives 0.9, 1.0 or 1.1) (9e15 / 21 = 4.2e14).
      //while (c0 < 9 && c0 != 1 || c0 == 1 && c.charAt(1) > 1) {
      // max n is 6 (gives 0.7 - 1.3)
      while (c0 < 7 && c0 != 1 || c0 == 1 && c.charAt(1) > 3) {
        x = x.times(y);
        c = digitsToString(x.d);
        c0 = c.charAt(0);
        n++;
      }

      e = x.e;

      if (c0 > 1) {
        x = new Ctor('0.' + c);
        e++;
      } else {
        x = new Ctor(c0 + '.' + c.slice(1));
      }
    } else {

      // The argument reduction method above may result in overflow if the argument y is a massive
      // number with exponent >= 1500000000000000 (9e15 / 6 = 1.5e15), so instead recall this
      // function using ln(x*10^e) = ln(x) + e*ln(10).
      t = getLn10(Ctor, wpr + 2, pr).times(e + '');
      x = naturalLogarithm(new Ctor(c0 + '.' + c.slice(1)), wpr - guard).plus(t);
      Ctor.precision = pr;

      return sd == null ? finalise(x, pr, rm, external = true) : x;
    }

    // x1 is x reduced to a value near 1.
    x1 = x;

    // Taylor series.
    // ln(y) = ln((1 + x)/(1 - x)) = 2(x + x^3/3 + x^5/5 + x^7/7 + ...)
    // where x = (y - 1)/(y + 1)    (|x| < 1)
    sum = numerator = x = divide(x.minus(1), x.plus(1), wpr, 1);
    x2 = finalise(x.times(x), wpr, 1);
    denominator = 3;

    for (;;) {
      numerator = finalise(numerator.times(x2), wpr, 1);
      t = sum.plus(divide(numerator, new Ctor(denominator), wpr, 1));

      if (digitsToString(t.d).slice(0, wpr) === digitsToString(sum.d).slice(0, wpr)) {
        sum = sum.times(2);

        // Reverse the argument reduction. Check that e is not 0 because, besides preventing an
        // unnecessary calculation, -0 + 0 = +0 and to ensure correct rounding -0 needs to stay -0.
        if (e !== 0) sum = sum.plus(getLn10(Ctor, wpr + 2, pr).times(e + ''));
        sum = divide(sum, new Ctor(n), wpr, 1);

        // Is rm > 3 and the first 4 rounding digits 4999, or rm < 4 (or the summation has
        // been repeated previously) and the first 4 rounding digits 9999?
        // If so, restart the summation with a higher precision, otherwise
        // e.g. with precision: 12, rounding: 1
        // ln(135520028.6126091714265381533) = 18.7246299999 when it should be 18.72463.
        // `wpr - guard` is the index of first rounding digit.
        if (sd == null) {
          if (checkRoundingDigits(sum.d, wpr - guard, rm, rep)) {
            Ctor.precision = wpr += guard;
            t = numerator = x = divide(x1.minus(1), x1.plus(1), wpr, 1);
            x2 = finalise(x.times(x), wpr, 1);
            denominator = rep = 1;
          } else {
            return finalise(sum, Ctor.precision = pr, rm, external = true);
          }
        } else {
          Ctor.precision = pr;
          return sum;
        }
      }

      sum = t;
      denominator += 2;
    }
  }


  // ±Infinity, NaN.
  function nonFiniteToString(x) {
    // Unsigned.
    return String(x.s * x.s / 0);
  }


  /*
   * Parse the value of a new Decimal `x` from string `str`.
   */
  function parseDecimal(x, str) {
    var e, i, len;

    // Decimal point?
    if ((e = str.indexOf('.')) > -1) str = str.replace('.', '');

    // Exponential form?
    if ((i = str.search(/e/i)) > 0) {

      // Determine exponent.
      if (e < 0) e = i;
      e += +str.slice(i + 1);
      str = str.substring(0, i);
    } else if (e < 0) {

      // Integer.
      e = str.length;
    }

    // Determine leading zeros.
    for (i = 0; str.charCodeAt(i) === 48; i++);

    // Determine trailing zeros.
    for (len = str.length; str.charCodeAt(len - 1) === 48; --len);
    str = str.slice(i, len);

    if (str) {
      len -= i;
      x.e = e = e - i - 1;
      x.d = [];

      // Transform base

      // e is the base 10 exponent.
      // i is where to slice str to get the first word of the digits array.
      i = (e + 1) % LOG_BASE;
      if (e < 0) i += LOG_BASE;

      if (i < len) {
        if (i) x.d.push(+str.slice(0, i));
        for (len -= LOG_BASE; i < len;) x.d.push(+str.slice(i, i += LOG_BASE));
        str = str.slice(i);
        i = LOG_BASE - str.length;
      } else {
        i -= len;
      }

      for (; i--;) str += '0';
      x.d.push(+str);

      if (external) {

        // Overflow?
        if (x.e > x.constructor.maxE) {

          // Infinity.
          x.d = null;
          x.e = NaN;

        // Underflow?
        } else if (x.e < x.constructor.minE) {

          // Zero.
          x.e = 0;
          x.d = [0];
          // x.constructor.underflow = true;
        } // else x.constructor.underflow = false;
      }
    } else {

      // Zero.
      x.e = 0;
      x.d = [0];
    }

    return x;
  }


  /*
   * Parse the value of a new Decimal `x` from a string `str`, which is not a decimal value.
   */
  function parseOther(x, str) {
    var base, Ctor, divisor, i, isFloat, len, p, xd, xe;

    if (str === 'Infinity' || str === 'NaN') {
      if (!+str) x.s = NaN;
      x.e = NaN;
      x.d = null;
      return x;
    }

    if (isHex.test(str))  {
      base = 16;
      str = str.toLowerCase();
    } else if (isBinary.test(str))  {
      base = 2;
    } else if (isOctal.test(str))  {
      base = 8;
    } else {
      throw Error(invalidArgument + str);
    }

    // Is there a binary exponent part?
    i = str.search(/p/i);

    if (i > 0) {
      p = +str.slice(i + 1);
      str = str.substring(2, i);
    } else {
      str = str.slice(2);
    }

    // Convert `str` as an integer then divide the result by `base` raised to a power such that the
    // fraction part will be restored.
    i = str.indexOf('.');
    isFloat = i >= 0;
    Ctor = x.constructor;

    if (isFloat) {
      str = str.replace('.', '');
      len = str.length;
      i = len - i;

      // log[10](16) = 1.2041... , log[10](88) = 1.9444....
      divisor = intPow(Ctor, new Ctor(base), i, i * 2);
    }

    xd = convertBase(str, base, BASE);
    xe = xd.length - 1;

    // Remove trailing zeros.
    for (i = xe; xd[i] === 0; --i) xd.pop();
    if (i < 0) return new Ctor(x.s * 0);
    x.e = getBase10Exponent(xd, xe);
    x.d = xd;
    external = false;

    // At what precision to perform the division to ensure exact conversion?
    // maxDecimalIntegerPartDigitCount = ceil(log[10](b) * otherBaseIntegerPartDigitCount)
    // log[10](2) = 0.30103, log[10](8) = 0.90309, log[10](16) = 1.20412
    // E.g. ceil(1.2 * 3) = 4, so up to 4 decimal digits are needed to represent 3 hex int digits.
    // maxDecimalFractionPartDigitCount = {Hex:4|Oct:3|Bin:1} * otherBaseFractionPartDigitCount
    // Therefore using 4 * the number of digits of str will always be enough.
    if (isFloat) x = divide(x, divisor, len * 4);

    // Multiply by the binary exponent part if present.
    if (p) x = x.times(Math.abs(p) < 54 ? Math.pow(2, p) : Decimal.pow(2, p));
    external = true;

    return x;
  }


  /*
   * sin(x) = x - x^3/3! + x^5/5! - ...
   * |x| < pi/2
   *
   */
  function sine(Ctor, x) {
    var k,
      len = x.d.length;

    if (len < 3) return taylorSeries(Ctor, 2, x, x);

    // Argument reduction: sin(5x) = 16*sin^5(x) - 20*sin^3(x) + 5*sin(x)
    // i.e. sin(x) = 16*sin^5(x/5) - 20*sin^3(x/5) + 5*sin(x/5)
    // and  sin(x) = sin(x/5)(5 + sin^2(x/5)(16sin^2(x/5) - 20))

    // Estimate the optimum number of times to use the argument reduction.
    k = 1.4 * Math.sqrt(len);
    k = k > 16 ? 16 : k | 0;

    // Max k before Math.pow precision loss is 22
    x = x.times(Math.pow(5, -k));
    x = taylorSeries(Ctor, 2, x, x);

    // Reverse argument reduction
    var sin2_x,
      d5 = new Ctor(5),
      d16 = new Ctor(16),
      d20 = new Ctor(20);
    for (; k--;) {
      sin2_x = x.times(x);
      x = x.times(d5.plus(sin2_x.times(d16.times(sin2_x).minus(d20))));
    }

    return x;
  }


  // Calculate Taylor series for `cos`, `cosh`, `sin` and `sinh`.
  function taylorSeries(Ctor, n, x, y, isHyperbolic) {
    var j, t, u, x2,
      i = 1,
      pr = Ctor.precision,
      k = Math.ceil(pr / LOG_BASE);

    external = false;
    x2 = x.times(x);
    u = new Ctor(y);

    for (;;) {
      t = divide(u.times(x2), new Ctor(n++ * n++), pr, 1);
      u = isHyperbolic ? y.plus(t) : y.minus(t);
      y = divide(t.times(x2), new Ctor(n++ * n++), pr, 1);
      t = u.plus(y);

      if (t.d[k] !== void 0) {
        for (j = k; t.d[j] === u.d[j] && j--;);
        if (j == -1) break;
      }

      j = u;
      u = y;
      y = t;
      t = j;
      i++;
    }

    external = true;
    t.d.length = k + 1;

    return t;
  }


  // Return the absolute value of `x` reduced to less than or equal to half pi.
  function toLessThanHalfPi(Ctor, x) {
    var t,
      isNeg = x.s < 0,
      pi = getPi(Ctor, Ctor.precision, 1),
      halfPi = pi.times(0.5);

    x = x.abs();

    if (x.lte(halfPi)) {
      quadrant = isNeg ? 4 : 1;
      return x;
    }

    t = x.divToInt(pi);

    if (t.isZero()) {
      quadrant = isNeg ? 3 : 2;
    } else {
      x = x.minus(t.times(pi));

      // 0 <= x < pi
      if (x.lte(halfPi)) {
        quadrant = isOdd(t) ? (isNeg ? 2 : 3) : (isNeg ? 4 : 1);
        return x;
      }

      quadrant = isOdd(t) ? (isNeg ? 1 : 4) : (isNeg ? 3 : 2);
    }

    return x.minus(pi).abs();
  }


  /*
   * Return the value of Decimal `x` as a string in base `baseOut`.
   *
   * If the optional `sd` argument is present include a binary exponent suffix.
   */
  function toStringBinary(x, baseOut, sd, rm) {
    var base, e, i, k, len, roundUp, str, xd, y,
      Ctor = x.constructor,
      isExp = sd !== void 0;

    if (isExp) {
      checkInt32(sd, 1, MAX_DIGITS);
      if (rm === void 0) rm = Ctor.rounding;
      else checkInt32(rm, 0, 8);
    } else {
      sd = Ctor.precision;
      rm = Ctor.rounding;
    }

    if (!x.isFinite()) {
      str = nonFiniteToString(x);
    } else {
      str = finiteToString(x);
      i = str.indexOf('.');

      // Use exponential notation according to `toExpPos` and `toExpNeg`? No, but if required:
      // maxBinaryExponent = floor((decimalExponent + 1) * log[2](10))
      // minBinaryExponent = floor(decimalExponent * log[2](10))
      // log[2](10) = 3.321928094887362347870319429489390175864

      if (isExp) {
        base = 2;
        if (baseOut == 16) {
          sd = sd * 4 - 3;
        } else if (baseOut == 8) {
          sd = sd * 3 - 2;
        }
      } else {
        base = baseOut;
      }

      // Convert the number as an integer then divide the result by its base raised to a power such
      // that the fraction part will be restored.

      // Non-integer.
      if (i >= 0) {
        str = str.replace('.', '');
        y = new Ctor(1);
        y.e = str.length - i;
        y.d = convertBase(finiteToString(y), 10, base);
        y.e = y.d.length;
      }

      xd = convertBase(str, 10, base);
      e = len = xd.length;

      // Remove trailing zeros.
      for (; xd[--len] == 0;) xd.pop();

      if (!xd[0]) {
        str = isExp ? '0p+0' : '0';
      } else {
        if (i < 0) {
          e--;
        } else {
          x = new Ctor(x);
          x.d = xd;
          x.e = e;
          x = divide(x, y, sd, rm, 0, base);
          xd = x.d;
          e = x.e;
          roundUp = inexact;
        }

        // The rounding digit, i.e. the digit after the digit that may be rounded up.
        i = xd[sd];
        k = base / 2;
        roundUp = roundUp || xd[sd + 1] !== void 0;

        roundUp = rm < 4
          ? (i !== void 0 || roundUp) && (rm === 0 || rm === (x.s < 0 ? 3 : 2))
          : i > k || i === k && (rm === 4 || roundUp || rm === 6 && xd[sd - 1] & 1 ||
            rm === (x.s < 0 ? 8 : 7));

        xd.length = sd;

        if (roundUp) {

          // Rounding up may mean the previous digit has to be rounded up and so on.
          for (; ++xd[--sd] > base - 1;) {
            xd[sd] = 0;
            if (!sd) {
              ++e;
              xd.unshift(1);
            }
          }
        }

        // Determine trailing zeros.
        for (len = xd.length; !xd[len - 1]; --len);

        // E.g. [4, 11, 15] becomes 4bf.
        for (i = 0, str = ''; i < len; i++) str += NUMERALS.charAt(xd[i]);

        // Add binary exponent suffix?
        if (isExp) {
          if (len > 1) {
            if (baseOut == 16 || baseOut == 8) {
              i = baseOut == 16 ? 4 : 3;
              for (--len; len % i; len++) str += '0';
              xd = convertBase(str, base, baseOut);
              for (len = xd.length; !xd[len - 1]; --len);

              // xd[0] will always be be 1
              for (i = 1, str = '1.'; i < len; i++) str += NUMERALS.charAt(xd[i]);
            } else {
              str = str.charAt(0) + '.' + str.slice(1);
            }
          }

          str =  str + (e < 0 ? 'p' : 'p+') + e;
        } else if (e < 0) {
          for (; ++e;) str = '0' + str;
          str = '0.' + str;
        } else {
          if (++e > len) for (e -= len; e-- ;) str += '0';
          else if (e < len) str = str.slice(0, e) + '.' + str.slice(e);
        }
      }

      str = (baseOut == 16 ? '0x' : baseOut == 2 ? '0b' : baseOut == 8 ? '0o' : '') + str;
    }

    return x.s < 0 ? '-' + str : str;
  }


  // Does not strip trailing zeros.
  function truncate(arr, len) {
    if (arr.length > len) {
      arr.length = len;
      return true;
    }
  }


  // Decimal methods


  /*
   *  abs
   *  acos
   *  acosh
   *  add
   *  asin
   *  asinh
   *  atan
   *  atanh
   *  atan2
   *  cbrt
   *  ceil
   *  clone
   *  config
   *  cos
   *  cosh
   *  div
   *  exp
   *  floor
   *  hypot
   *  ln
   *  log
   *  log2
   *  log10
   *  max
   *  min
   *  mod
   *  mul
   *  pow
   *  random
   *  round
   *  set
   *  sign
   *  sin
   *  sinh
   *  sqrt
   *  sub
   *  tan
   *  tanh
   *  trunc
   */


  /*
   * Return a new Decimal whose value is the absolute value of `x`.
   *
   * x {number|string|Decimal}
   *
   */
  function abs(x) {
    return new this(x).abs();
  }


  /*
   * Return a new Decimal whose value is the arccosine in radians of `x`.
   *
   * x {number|string|Decimal}
   *
   */
  function acos(x) {
    return new this(x).acos();
  }


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic cosine of `x`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function acosh(x) {
    return new this(x).acosh();
  }


  /*
   * Return a new Decimal whose value is the sum of `x` and `y`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function add(x, y) {
    return new this(x).plus(y);
  }


  /*
   * Return a new Decimal whose value is the arcsine in radians of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function asin(x) {
    return new this(x).asin();
  }


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic sine of `x`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function asinh(x) {
    return new this(x).asinh();
  }


  /*
   * Return a new Decimal whose value is the arctangent in radians of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function atan(x) {
    return new this(x).atan();
  }


  /*
   * Return a new Decimal whose value is the inverse of the hyperbolic tangent of `x`, rounded to
   * `precision` significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function atanh(x) {
    return new this(x).atanh();
  }


  /*
   * Return a new Decimal whose value is the arctangent in radians of `y/x` in the range -pi to pi
   * (inclusive), rounded to `precision` significant digits using rounding mode `rounding`.
   *
   * Domain: [-Infinity, Infinity]
   * Range: [-pi, pi]
   *
   * y {number|string|Decimal} The y-coordinate.
   * x {number|string|Decimal} The x-coordinate.
   *
   * atan2(±0, -0)               = ±pi
   * atan2(±0, +0)               = ±0
   * atan2(±0, -x)               = ±pi for x > 0
   * atan2(±0, x)                = ±0 for x > 0
   * atan2(-y, ±0)               = -pi/2 for y > 0
   * atan2(y, ±0)                = pi/2 for y > 0
   * atan2(±y, -Infinity)        = ±pi for finite y > 0
   * atan2(±y, +Infinity)        = ±0 for finite y > 0
   * atan2(±Infinity, x)         = ±pi/2 for finite x
   * atan2(±Infinity, -Infinity) = ±3*pi/4
   * atan2(±Infinity, +Infinity) = ±pi/4
   * atan2(NaN, x) = NaN
   * atan2(y, NaN) = NaN
   *
   */
  function atan2(y, x) {
    y = new this(y);
    x = new this(x);
    var r,
      pr = this.precision,
      rm = this.rounding,
      wpr = pr + 4;

    // Either NaN
    if (!y.s || !x.s) {
      r = new this(NaN);

    // Both ±Infinity
    } else if (!y.d && !x.d) {
      r = getPi(this, wpr, 1).times(x.s > 0 ? 0.25 : 0.75);
      r.s = y.s;

    // x is ±Infinity or y is ±0
    } else if (!x.d || y.isZero()) {
      r = x.s < 0 ? getPi(this, pr, rm) : new this(0);
      r.s = y.s;

    // y is ±Infinity or x is ±0
    } else if (!y.d || x.isZero()) {
      r = getPi(this, wpr, 1).times(0.5);
      r.s = y.s;

    // Both non-zero and finite
    } else if (x.s < 0) {
      this.precision = wpr;
      this.rounding = 1;
      r = this.atan(divide(y, x, wpr, 1));
      x = getPi(this, wpr, 1);
      this.precision = pr;
      this.rounding = rm;
      r = y.s < 0 ? r.minus(x) : r.plus(x);
    } else {
      r = this.atan(divide(y, x, wpr, 1));
    }

    return r;
  }


  /*
   * Return a new Decimal whose value is the cube root of `x`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function cbrt(x) {
    return new this(x).cbrt();
  }


  /*
   * Return a new Decimal whose value is `x` rounded to an integer using `ROUND_CEIL`.
   *
   * x {number|string|Decimal}
   *
   */
  function ceil(x) {
    return finalise(x = new this(x), x.e + 1, 2);
  }


  /*
   * Configure global settings for a Decimal constructor.
   *
   * `obj` is an object with one or more of the following properties,
   *
   *   precision  {number}
   *   rounding   {number}
   *   toExpNeg   {number}
   *   toExpPos   {number}
   *   maxE       {number}
   *   minE       {number}
   *   modulo     {number}
   *   crypto     {boolean|number}
   *
   * E.g. Decimal.config({ precision: 20, rounding: 4 })
   *
   */
  function config(obj) {
    if (!obj || typeof obj !== 'object') throw Error(decimalError + 'Object expected');
    var i, p, v,
      ps = [
        'precision', 1, MAX_DIGITS,
        'rounding', 0, 8,
        'toExpNeg', -EXP_LIMIT, 0,
        'toExpPos', 0, EXP_LIMIT,
        'maxE', 0, EXP_LIMIT,
        'minE', -EXP_LIMIT, 0,
        'modulo', 0, 9
      ];

    for (i = 0; i < ps.length; i += 3) {
      if ((v = obj[p = ps[i]]) !== void 0) {
        if (mathfloor(v) === v && v >= ps[i + 1] && v <= ps[i + 2]) this[p] = v;
        else throw Error(invalidArgument + p + ': ' + v);
      }
    }

    if ((v = obj[p = 'crypto']) !== void 0) {
      if (v === true || v === false || v === 0 || v === 1) {
        if (v) {
          if (typeof crypto != 'undefined' && crypto &&
            (crypto.getRandomValues || crypto.randomBytes)) {
            this[p] = true;
          } else {
            throw Error(cryptoUnavailable);
          }
        } else {
          this[p] = false;
        }
      } else {
        throw Error(invalidArgument + p + ': ' + v);
      }
    }

    return this;
  }


  /*
   * Return a new Decimal whose value is the cosine of `x`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function cos(x) {
    return new this(x).cos();
  }


  /*
   * Return a new Decimal whose value is the hyperbolic cosine of `x`, rounded to precision
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function cosh(x) {
    return new this(x).cosh();
  }


  /*
   * Create and return a Decimal constructor with the same configuration properties as this Decimal
   * constructor.
   *
   */
  function clone(obj) {
    var i, p, ps;

    /*
     * The Decimal constructor and exported function.
     * Return a new Decimal instance.
     *
     * v {number|string|Decimal} A numeric value.
     *
     */
    function Decimal(v) {
      var e, i, t,
        x = this;

      // Decimal called without new.
      if (!(x instanceof Decimal)) return new Decimal(v);

      // Retain a reference to this Decimal constructor, and shadow Decimal.prototype.constructor
      // which points to Object.
      x.constructor = Decimal;

      // Duplicate.
      if (v instanceof Decimal) {
        x.s = v.s;
        x.e = v.e;
        x.d = (v = v.d) ? v.slice() : v;
        return;
      }

      t = typeof v;

      if (t === 'number') {
        if (v === 0) {
          x.s = 1 / v < 0 ? -1 : 1;
          x.e = 0;
          x.d = [0];
          return;
        }

        if (v < 0) {
          v = -v;
          x.s = -1;
        } else {
          x.s = 1;
        }

        // Fast path for small integers.
        if (v === ~~v && v < 1e7) {
          for (e = 0, i = v; i >= 10; i /= 10) e++;
          x.e = e;
          x.d = [v];
          return;

        // Infinity, NaN.
        } else if (v * 0 !== 0) {
          if (!v) x.s = NaN;
          x.e = NaN;
          x.d = null;
          return;
        }

        return parseDecimal(x, v.toString());

      } else if (t !== 'string') {
        throw Error(invalidArgument + v);
      }

      // Minus sign?
      if (v.charCodeAt(0) === 45) {
        v = v.slice(1);
        x.s = -1;
      } else {
        x.s = 1;
      }

      return isDecimal.test(v) ? parseDecimal(x, v) : parseOther(x, v);
    }

    Decimal.prototype = P;

    Decimal.ROUND_UP = 0;
    Decimal.ROUND_DOWN = 1;
    Decimal.ROUND_CEIL = 2;
    Decimal.ROUND_FLOOR = 3;
    Decimal.ROUND_HALF_UP = 4;
    Decimal.ROUND_HALF_DOWN = 5;
    Decimal.ROUND_HALF_EVEN = 6;
    Decimal.ROUND_HALF_CEIL = 7;
    Decimal.ROUND_HALF_FLOOR = 8;
    Decimal.EUCLID = 9;

    Decimal.config = Decimal.set = config;
    Decimal.clone = clone;

    Decimal.abs = abs;
    Decimal.acos = acos;
    Decimal.acosh = acosh;        // ES6
    Decimal.add = add;
    Decimal.asin = asin;
    Decimal.asinh = asinh;        // ES6
    Decimal.atan = atan;
    Decimal.atanh = atanh;        // ES6
    Decimal.atan2 = atan2;
    Decimal.cbrt = cbrt;          // ES6
    Decimal.ceil = ceil;
    Decimal.cos = cos;
    Decimal.cosh = cosh;          // ES6
    Decimal.div = div;
    Decimal.exp = exp;
    Decimal.floor = floor;
    Decimal.hypot = hypot;        // ES6
    Decimal.ln = ln;
    Decimal.log = log;
    Decimal.log10 = log10;        // ES6
    Decimal.log2 = log2;          // ES6
    Decimal.max = max;
    Decimal.min = min;
    Decimal.mod = mod;
    Decimal.mul = mul;
    Decimal.pow = pow;
    Decimal.random = random;
    Decimal.round = round;
    Decimal.sign = sign;          // ES6
    Decimal.sin = sin;
    Decimal.sinh = sinh;          // ES6
    Decimal.sqrt = sqrt;
    Decimal.sub = sub;
    Decimal.tan = tan;
    Decimal.tanh = tanh;          // ES6
    Decimal.trunc = trunc;        // ES6

    if (obj === void 0) obj = {};
    if (obj) {
      ps = ['precision', 'rounding', 'toExpNeg', 'toExpPos', 'maxE', 'minE', 'modulo', 'crypto'];
      for (i = 0; i < ps.length;) if (!obj.hasOwnProperty(p = ps[i++])) obj[p] = this[p];
    }

    Decimal.config(obj);

    return Decimal;
  }


  /*
   * Return a new Decimal whose value is `x` divided by `y`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function div(x, y) {
    return new this(x).div(y);
  }


  /*
   * Return a new Decimal whose value is the natural exponential of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} The power to which to raise the base of the natural log.
   *
   */
  function exp(x) {
    return new this(x).exp();
  }


  /*
   * Return a new Decimal whose value is `x` round to an integer using `ROUND_FLOOR`.
   *
   * x {number|string|Decimal}
   *
   */
  function floor(x) {
    return finalise(x = new this(x), x.e + 1, 3);
  }


  /*
   * Return a new Decimal whose value is the square root of the sum of the squares of the arguments,
   * rounded to `precision` significant digits using rounding mode `rounding`.
   *
   * hypot(a, b, ...) = sqrt(a^2 + b^2 + ...)
   *
   */
  function hypot() {
    var i, n,
      t = new this(0);

    external = false;

    for (i = 0; i < arguments.length;) {
      n = new this(arguments[i++]);
      if (!n.d) {
        if (n.s) {
          external = true;
          return new this(1 / 0);
        }
        t = n;
      } else if (t.d) {
        t = t.plus(n.times(n));
      }
    }

    external = true;

    return t.sqrt();
  }


  /*
   * Return a new Decimal whose value is the natural logarithm of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function ln(x) {
    return new this(x).ln();
  }


  /*
   * Return a new Decimal whose value is the log of `x` to the base `y`, or to base 10 if no base
   * is specified, rounded to `precision` significant digits using rounding mode `rounding`.
   *
   * log[y](x)
   *
   * x {number|string|Decimal} The argument of the logarithm.
   * y {number|string|Decimal} The base of the logarithm.
   *
   */
  function log(x, y) {
    return new this(x).log(y);
  }


  /*
   * Return a new Decimal whose value is the base 2 logarithm of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function log2(x) {
    return new this(x).log(2);
  }


  /*
   * Return a new Decimal whose value is the base 10 logarithm of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function log10(x) {
    return new this(x).log(10);
  }


  /*
   * Return a new Decimal whose value is the maximum of the arguments.
   *
   * arguments {number|string|Decimal}
   *
   */
  function max() {
    return maxOrMin(this, arguments, 'lt');
  }


  /*
   * Return a new Decimal whose value is the minimum of the arguments.
   *
   * arguments {number|string|Decimal}
   *
   */
  function min() {
    return maxOrMin(this, arguments, 'gt');
  }


  /*
   * Return a new Decimal whose value is `x` modulo `y`, rounded to `precision` significant digits
   * using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function mod(x, y) {
    return new this(x).mod(y);
  }


  /*
   * Return a new Decimal whose value is `x` multiplied by `y`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function mul(x, y) {
    return new this(x).mul(y);
  }


  /*
   * Return a new Decimal whose value is `x` raised to the power `y`, rounded to precision
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} The base.
   * y {number|string|Decimal} The exponent.
   *
   */
  function pow(x, y) {
    return new this(x).pow(y);
  }


  /*
   * Returns a new Decimal with a random value equal to or greater than 0 and less than 1, and with
   * `sd`, or `Decimal.precision` if `sd` is omitted, significant digits (or less if trailing zeros
   * are produced).
   *
   * [sd] {number} Significant digits. Integer, 0 to MAX_DIGITS inclusive.
   *
   */
  function random(sd) {
    var d, e, k, n,
      i = 0,
      r = new this(1),
      rd = [];

    if (sd === void 0) sd = this.precision;
    else checkInt32(sd, 1, MAX_DIGITS);

    k = Math.ceil(sd / LOG_BASE);

    if (!this.crypto) {
      for (; i < k;) rd[i++] = Math.random() * 1e7 | 0;

    // Browsers supporting crypto.getRandomValues.
    } else if (crypto.getRandomValues) {
      d = crypto.getRandomValues(new Uint32Array(k));

      for (; i < k;) {
        n = d[i];

        // 0 <= n < 4294967296
        // Probability n >= 4.29e9, is 4967296 / 4294967296 = 0.00116 (1 in 865).
        if (n >= 4.29e9) {
          d[i] = crypto.getRandomValues(new Uint32Array(1))[0];
        } else {

          // 0 <= n <= 4289999999
          // 0 <= (n % 1e7) <= 9999999
          rd[i++] = n % 1e7;
        }
      }

    // Node.js supporting crypto.randomBytes.
    } else if (crypto.randomBytes) {

      // buffer
      d = crypto.randomBytes(k *= 4);

      for (; i < k;) {

        // 0 <= n < 2147483648
        n = d[i] + (d[i + 1] << 8) + (d[i + 2] << 16) + ((d[i + 3] & 0x7f) << 24);

        // Probability n >= 2.14e9, is 7483648 / 2147483648 = 0.0035 (1 in 286).
        if (n >= 2.14e9) {
          crypto.randomBytes(4).copy(d, i);
        } else {

          // 0 <= n <= 2139999999
          // 0 <= (n % 1e7) <= 9999999
          rd.push(n % 1e7);
          i += 4;
        }
      }

      i = k / 4;
    } else {
      throw Error(cryptoUnavailable);
    }

    k = rd[--i];
    sd %= LOG_BASE;

    // Convert trailing digits to zeros according to sd.
    if (k && sd) {
      n = mathpow(10, LOG_BASE - sd);
      rd[i] = (k / n | 0) * n;
    }

    // Remove trailing words which are zero.
    for (; rd[i] === 0; i--) rd.pop();

    // Zero?
    if (i < 0) {
      e = 0;
      rd = [0];
    } else {
      e = -1;

      // Remove leading words which are zero and adjust exponent accordingly.
      for (; rd[0] === 0; e -= LOG_BASE) rd.shift();

      // Count the digits of the first word of rd to determine leading zeros.
      for (k = 1, n = rd[0]; n >= 10; n /= 10) k++;

      // Adjust the exponent for leading zeros of the first word of rd.
      if (k < LOG_BASE) e -= LOG_BASE - k;
    }

    r.e = e;
    r.d = rd;

    return r;
  }


  /*
   * Return a new Decimal whose value is `x` rounded to an integer using rounding mode `rounding`.
   *
   * To emulate `Math.round`, set rounding to 7 (ROUND_HALF_CEIL).
   *
   * x {number|string|Decimal}
   *
   */
  function round(x) {
    return finalise(x = new this(x), x.e + 1, this.rounding);
  }


  /*
   * Return
   *   1    if x > 0,
   *  -1    if x < 0,
   *   0    if x is 0,
   *  -0    if x is -0,
   *   NaN  otherwise
   *
   */
  function sign(x) {
    x = new this(x);
    return x.d ? (x.d[0] ? x.s : 0 * x.s) : x.s || NaN;
  }


  /*
   * Return a new Decimal whose value is the sine of `x`, rounded to `precision` significant digits
   * using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function sin(x) {
    return new this(x).sin();
  }


  /*
   * Return a new Decimal whose value is the hyperbolic sine of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function sinh(x) {
    return new this(x).sinh();
  }


  /*
   * Return a new Decimal whose value is the square root of `x`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   *
   */
  function sqrt(x) {
    return new this(x).sqrt();
  }


  /*
   * Return a new Decimal whose value is `x` minus `y`, rounded to `precision` significant digits
   * using rounding mode `rounding`.
   *
   * x {number|string|Decimal}
   * y {number|string|Decimal}
   *
   */
  function sub(x, y) {
    return new this(x).sub(y);
  }


  /*
   * Return a new Decimal whose value is the tangent of `x`, rounded to `precision` significant
   * digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function tan(x) {
    return new this(x).tan();
  }


  /*
   * Return a new Decimal whose value is the hyperbolic tangent of `x`, rounded to `precision`
   * significant digits using rounding mode `rounding`.
   *
   * x {number|string|Decimal} A value in radians.
   *
   */
  function tanh(x) {
    return new this(x).tanh();
  }


  /*
   * Return a new Decimal whose value is `x` truncated to an integer.
   *
   * x {number|string|Decimal}
   *
   */
  function trunc(x) {
    return finalise(x = new this(x), x.e + 1, 1);
  }


  // Create and configure initial Decimal constructor.
  Decimal = clone(Decimal);

  Decimal['default'] = Decimal.Decimal = Decimal;

  // Create the internal constants from their string values.
  LN10 = new Decimal(LN10);
  PI = new Decimal(PI);


  // Export.


  // AMD.
  if (typeof define == 'function' && define.amd) {
    define(function () {
      return Decimal;
    });

  // Node and other environments that support module.exports.
  } else if (typeof module != 'undefined' && module.exports) {
    module.exports = Decimal;

  // Browser.
  } else {
    if (!globalScope) {
      globalScope = typeof self != 'undefined' && self && self.self == self
        ? self : Function('return this')();
    }

    noConflict = globalScope.Decimal;
    Decimal.noConflict = function () {
      globalScope.Decimal = noConflict;
      return Decimal;
    };

    globalScope.Decimal = Decimal;
  }
})(this);

},{}],2:[function(require,module,exports){
// Generated by purs bundle 0.13.8
var PS = {};
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Alt"] = $PS["Control.Alt"] || {};
  var exports = $PS["Control.Alt"];                          
  var Alt = function (Functor0, alt) {
      this.Functor0 = Functor0;
      this.alt = alt;
  };                                                       
  var alt = function (dict) {
      return dict.alt;
  };
  exports["Alt"] = Alt;
  exports["alt"] = alt;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Alternative"] = $PS["Control.Alternative"] || {};
  var exports = $PS["Control.Alternative"];              
  var Alternative = function (Applicative0, Plus1) {
      this.Applicative0 = Applicative0;
      this.Plus1 = Plus1;
  };
  exports["Alternative"] = Alternative;
})(PS);
(function(exports) {
  "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Unit"] = $PS["Data.Unit"] || {};
  var exports = $PS["Data.Unit"];
  var $foreign = $PS["Data.Unit"];
  exports["unit"] = $foreign.unit;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Applicative"] = $PS["Control.Applicative"] || {};
  var exports = $PS["Control.Applicative"];
  var Data_Unit = $PS["Data.Unit"];                
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var unless = function (dictApplicative) {
      return function (v) {
          return function (v1) {
              if (!v) {
                  return v1;
              };
              if (v) {
                  return pure(dictApplicative)(Data_Unit.unit);
              };
              throw new Error("Failed pattern match at Control.Applicative (line 62, column 1 - line 62, column 65): " + [ v.constructor.name, v1.constructor.name ]);
          };
      };
  };
  var when = function (dictApplicative) {
      return function (v) {
          return function (v1) {
              if (v) {
                  return v1;
              };
              if (!v) {
                  return pure(dictApplicative)(Data_Unit.unit);
              };
              throw new Error("Failed pattern match at Control.Applicative (line 57, column 1 - line 57, column 63): " + [ v.constructor.name, v1.constructor.name ]);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["unless"] = unless;
  exports["when"] = when;
})(PS);
(function(exports) {
  "use strict";

  exports.arrayApply = function (fs) {
    return function (xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l*k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Semigroupoid"] = $PS["Control.Semigroupoid"] || {};
  var exports = $PS["Control.Semigroupoid"];
  var Semigroupoid = function (compose) {
      this.compose = compose;
  };
  var semigroupoidFn = new Semigroupoid(function (f) {
      return function (g) {
          return function (x) {
              return f(g(x));
          };
      };
  });
  var compose = function (dict) {
      return dict.compose;
  };
  exports["compose"] = compose;
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Category"] = $PS["Control.Category"] || {};
  var exports = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];                
  var Category = function (Semigroupoid0, identity) {
      this.Semigroupoid0 = Semigroupoid0;
      this.identity = identity;
  };
  var identity = function (dict) {
      return dict.identity;
  };
  var categoryFn = new Category(function () {
      return Control_Semigroupoid.semigroupoidFn;
  }, function (x) {
      return x;
  });
  exports["identity"] = identity;
  exports["categoryFn"] = categoryFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Function"] = $PS["Data.Function"] || {};
  var exports = $PS["Data.Function"];                    
  var on = function (f) {
      return function (g) {
          return function (x) {
              return function (y) {
                  return f(g(x))(g(y));
              };
          };
      };
  };
  var flip = function (f) {
      return function (b) {
          return function (a) {
              return f(a)(b);
          };
      };
  };
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["flip"] = flip;
  exports["const"] = $$const;
  exports["on"] = on;
})(PS);
(function(exports) {
  "use strict";

  exports.arrayMap = function (f) {
    return function (arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Functor"] = $PS["Data.Functor"] || {};
  var exports = $PS["Data.Functor"];
  var $foreign = $PS["Data.Functor"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Function = $PS["Data.Function"];
  var Data_Unit = $PS["Data.Unit"];                
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };
  var voidLeft = function (dictFunctor) {
      return function (f) {
          return function (x) {
              return map(dictFunctor)(Data_Function["const"](x))(f);
          };
      };
  };
  var functorFn = new Functor(Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn));
  var functorArray = new Functor($foreign.arrayMap);
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
  exports["voidLeft"] = voidLeft;
  exports["functorFn"] = functorFn;
  exports["functorArray"] = functorArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Apply"] = $PS["Control.Apply"] || {};
  var exports = $PS["Control.Apply"];
  var $foreign = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];                
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  }; 
  var applyArray = new Apply(function () {
      return Data_Functor.functorArray;
  }, $foreign.arrayApply);
  var apply = function (dict) {
      return dict.apply;
  };
  var applyFirst = function (dictApply) {
      return function (a) {
          return function (b) {
              return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"])(a))(b);
          };
      };
  };
  var applySecond = function (dictApply) {
      return function (a) {
          return function (b) {
              return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"](Control_Category.identity(Control_Category.categoryFn)))(a))(b);
          };
      };
  };
  var lift2 = function (dictApply) {
      return function (f) {
          return function (a) {
              return function (b) {
                  return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b);
              };
          };
      };
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
  exports["applyFirst"] = applyFirst;
  exports["applySecond"] = applySecond;
  exports["lift2"] = lift2;
  exports["applyArray"] = applyArray;
})(PS);
(function(exports) {
  "use strict";

  exports.arrayBind = function (arr) {
    return function (f) {
      var result = [];
      for (var i = 0, l = arr.length; i < l; i++) {
        Array.prototype.push.apply(result, f(arr[i]));
      }
      return result;
    };
  };
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Bind"] = $PS["Control.Bind"] || {};
  var exports = $PS["Control.Bind"];
  var $foreign = $PS["Control.Bind"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Function = $PS["Data.Function"];                
  var Discard = function (discard) {
      this.discard = discard;
  };
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };
  var discard = function (dict) {
      return dict.discard;
  }; 
  var bindArray = new Bind(function () {
      return Control_Apply.applyArray;
  }, $foreign.arrayBind);
  var bind = function (dict) {
      return dict.bind;
  };
  var bindFlipped = function (dictBind) {
      return Data_Function.flip(bind(dictBind));
  };
  var composeKleisli = function (dictBind) {
      return function (f) {
          return function (g) {
              return function (a) {
                  return bind(dictBind)(f(a))(g);
              };
          };
      };
  };
  var discardUnit = new Discard(function (dictBind) {
      return bind(dictBind);
  });
  exports["Bind"] = Bind;
  exports["bind"] = bind;
  exports["bindFlipped"] = bindFlipped;
  exports["discard"] = discard;
  exports["composeKleisli"] = composeKleisli;
  exports["bindArray"] = bindArray;
  exports["discardUnit"] = discardUnit;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Lazy"] = $PS["Control.Lazy"] || {};
  var exports = $PS["Control.Lazy"];               
  var Lazy = function (defer) {
      this.defer = defer;
  }; 
  var defer = function (dict) {
      return dict.defer;
  };
  var fix = function (dictLazy) {
      return function (f) {
          var go = defer(dictLazy)(function (v) {
              return f(go);
          });
          return go;
      };
  };
  exports["defer"] = defer;
  exports["Lazy"] = Lazy;
  exports["fix"] = fix;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Monad"] = $PS["Control.Monad"] || {};
  var exports = $PS["Control.Monad"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];                
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (f$prime) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (a$prime) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(f$prime(a$prime));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Monad.Error.Class"] = $PS["Control.Monad.Error.Class"] || {};
  var exports = $PS["Control.Monad.Error.Class"];                
  var MonadThrow = function (Monad0, throwError) {
      this.Monad0 = Monad0;
      this.throwError = throwError;
  };
  var throwError = function (dict) {
      return dict.throwError;
  };
  exports["throwError"] = throwError;
  exports["MonadThrow"] = MonadThrow;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Tuple"] = $PS["Data.Tuple"] || {};
  var exports = $PS["Data.Tuple"];                         
  var Tuple = (function () {
      function Tuple(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Tuple.create = function (value0) {
          return function (value1) {
              return new Tuple(value0, value1);
          };
      };
      return Tuple;
  })();
  var snd = function (v) {
      return v.value1;
  };                                                                                                    
  var fst = function (v) {
      return v.value0;
  };
  exports["Tuple"] = Tuple;
  exports["fst"] = fst;
  exports["snd"] = snd;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Monad.State.Class"] = $PS["Control.Monad.State.Class"] || {};
  var exports = $PS["Control.Monad.State.Class"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];                
  var MonadState = function (Monad0, state) {
      this.Monad0 = Monad0;
      this.state = state;
  };
  var state = function (dict) {
      return dict.state;
  };
  var modify_ = function (dictMonadState) {
      return function (f) {
          return state(dictMonadState)(function (s) {
              return new Data_Tuple.Tuple(Data_Unit.unit, f(s));
          });
      };
  };
  var gets = function (dictMonadState) {
      return function (f) {
          return state(dictMonadState)(function (s) {
              return new Data_Tuple.Tuple(f(s), s);
          });
      };
  };
  exports["state"] = state;
  exports["MonadState"] = MonadState;
  exports["gets"] = gets;
  exports["modify_"] = modify_;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Monad.Trans.Class"] = $PS["Control.Monad.Trans.Class"] || {};
  var exports = $PS["Control.Monad.Trans.Class"];
  var MonadTrans = function (lift) {
      this.lift = lift;
  };
  var lift = function (dict) {
      return dict.lift;
  };
  exports["lift"] = lift;
  exports["MonadTrans"] = MonadTrans;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Bifunctor"] = $PS["Data.Bifunctor"] || {};
  var exports = $PS["Data.Bifunctor"];
  var Control_Category = $PS["Control.Category"];                
  var Bifunctor = function (bimap) {
      this.bimap = bimap;
  };
  var bimap = function (dict) {
      return dict.bimap;
  };
  var lmap = function (dictBifunctor) {
      return function (f) {
          return bimap(dictBifunctor)(f)(Control_Category.identity(Control_Category.categoryFn));
      };
  };
  exports["Bifunctor"] = Bifunctor;
  exports["lmap"] = lmap;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Either"] = $PS["Data.Either"] || {};
  var exports = $PS["Data.Either"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];          
  var Left = (function () {
      function Left(value0) {
          this.value0 = value0;
      };
      Left.create = function (value0) {
          return new Left(value0);
      };
      return Left;
  })();
  var Right = (function () {
      function Right(value0) {
          this.value0 = value0;
      };
      Right.create = function (value0) {
          return new Right(value0);
      };
      return Right;
  })();
  var functorEither = new Data_Functor.Functor(function (f) {
      return function (m) {
          if (m instanceof Left) {
              return new Left(m.value0);
          };
          if (m instanceof Right) {
              return new Right(f(m.value0));
          };
          throw new Error("Failed pattern match at Data.Either (line 38, column 1 - line 38, column 52): " + [ m.constructor.name ]);
      };
  });
  var either = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Left) {
                  return v(v2.value0);
              };
              if (v2 instanceof Right) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Either (line 238, column 1 - line 238, column 64): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };                                                                               
  var isRight = either(Data_Function["const"](false))(Data_Function["const"](true));
  var bifunctorEither = new Data_Bifunctor.Bifunctor(function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Left) {
                  return new Left(v(v2.value0));
              };
              if (v2 instanceof Right) {
                  return new Right(v1(v2.value0));
              };
              throw new Error("Failed pattern match at Data.Either (line 46, column 1 - line 48, column 36): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  });
  var applyEither = new Control_Apply.Apply(function () {
      return functorEither;
  }, function (v) {
      return function (v1) {
          if (v instanceof Left) {
              return new Left(v.value0);
          };
          if (v instanceof Right) {
              return Data_Functor.map(functorEither)(v.value0)(v1);
          };
          throw new Error("Failed pattern match at Data.Either (line 82, column 1 - line 84, column 30): " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var bindEither = new Control_Bind.Bind(function () {
      return applyEither;
  }, either(function (e) {
      return function (v) {
          return new Left(e);
      };
  })(function (a) {
      return function (f) {
          return f(a);
      };
  }));
  var applicativeEither = new Control_Applicative.Applicative(function () {
      return applyEither;
  }, Right.create);
  var monadEither = new Control_Monad.Monad(function () {
      return applicativeEither;
  }, function () {
      return bindEither;
  });
  exports["Left"] = Left;
  exports["Right"] = Right;
  exports["either"] = either;
  exports["isRight"] = isRight;
  exports["functorEither"] = functorEither;
  exports["bifunctorEither"] = bifunctorEither;
  exports["applicativeEither"] = applicativeEither;
  exports["bindEither"] = bindEither;
  exports["monadEither"] = monadEither;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Monad.Except.Trans"] = $PS["Control.Monad.Except.Trans"] || {};
  var exports = $PS["Control.Monad.Except.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];                
  var ExceptT = function (x) {
      return x;
  };
  var runExceptT = function (v) {
      return v;
  };          
  var monadTransExceptT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
      return function (m) {
          return Control_Bind.bind(dictMonad.Bind1())(m)(function (a) {
              return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(a));
          });
      };
  });
  var mapExceptT = function (f) {
      return function (v) {
          return f(v);
      };
  };
  var functorExceptT = function (dictFunctor) {
      return new Data_Functor.Functor(function (f) {
          return mapExceptT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Either.functorEither)(f)));
      });
  };
  var monadExceptT = function (dictMonad) {
      return new Control_Monad.Monad(function () {
          return applicativeExceptT(dictMonad);
      }, function () {
          return bindExceptT(dictMonad);
      });
  };
  var bindExceptT = function (dictMonad) {
      return new Control_Bind.Bind(function () {
          return applyExceptT(dictMonad);
      }, function (v) {
          return function (k) {
              return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either((function () {
                  var $90 = Control_Applicative.pure(dictMonad.Applicative0());
                  return function ($91) {
                      return $90(Data_Either.Left.create($91));
                  };
              })())(function (a) {
                  var v1 = k(a);
                  return v1;
              }));
          };
      });
  };
  var applyExceptT = function (dictMonad) {
      return new Control_Apply.Apply(function () {
          return functorExceptT(((dictMonad.Bind1()).Apply0()).Functor0());
      }, Control_Monad.ap(monadExceptT(dictMonad)));
  };
  var applicativeExceptT = function (dictMonad) {
      return new Control_Applicative.Applicative(function () {
          return applyExceptT(dictMonad);
      }, (function () {
          var $92 = Control_Applicative.pure(dictMonad.Applicative0());
          return function ($93) {
              return ExceptT($92(Data_Either.Right.create($93)));
          };
      })());
  };
  var monadStateExceptT = function (dictMonadState) {
      return new Control_Monad_State_Class.MonadState(function () {
          return monadExceptT(dictMonadState.Monad0());
      }, function (f) {
          return Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(f));
      });
  };
  var monadThrowExceptT = function (dictMonad) {
      return new Control_Monad_Error_Class.MonadThrow(function () {
          return monadExceptT(dictMonad);
      }, (function () {
          var $102 = Control_Applicative.pure(dictMonad.Applicative0());
          return function ($103) {
              return ExceptT($102(Data_Either.Left.create($103)));
          };
      })());
  };
  exports["ExceptT"] = ExceptT;
  exports["runExceptT"] = runExceptT;
  exports["functorExceptT"] = functorExceptT;
  exports["applyExceptT"] = applyExceptT;
  exports["applicativeExceptT"] = applicativeExceptT;
  exports["bindExceptT"] = bindExceptT;
  exports["monadThrowExceptT"] = monadThrowExceptT;
  exports["monadStateExceptT"] = monadStateExceptT;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Monad.State.Trans"] = $PS["Control.Monad.State.Trans"] || {};
  var exports = $PS["Control.Monad.State.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];                      
  var StateT = function (x) {
      return x;
  };
  var runStateT = function (v) {
      return v;
  };
  var lazyStateT = new Control_Lazy.Lazy(function (f) {
      return function (s) {
          var v = f(Data_Unit.unit);
          return v(s);
      };
  });
  var functorStateT = function (dictFunctor) {
      return new Data_Functor.Functor(function (f) {
          return function (v) {
              return function (s) {
                  return Data_Functor.map(dictFunctor)(function (v1) {
                      return new Data_Tuple.Tuple(f(v1.value0), v1.value1);
                  })(v(s));
              };
          };
      });
  };
  var evalStateT = function (dictFunctor) {
      return function (v) {
          return function (s) {
              return Data_Functor.map(dictFunctor)(Data_Tuple.fst)(v(s));
          };
      };
  };
  var monadStateT = function (dictMonad) {
      return new Control_Monad.Monad(function () {
          return applicativeStateT(dictMonad);
      }, function () {
          return bindStateT(dictMonad);
      });
  };
  var bindStateT = function (dictMonad) {
      return new Control_Bind.Bind(function () {
          return applyStateT(dictMonad);
      }, function (v) {
          return function (f) {
              return function (s) {
                  return Control_Bind.bind(dictMonad.Bind1())(v(s))(function (v1) {
                      var v3 = f(v1.value0);
                      return v3(v1.value1);
                  });
              };
          };
      });
  };
  var applyStateT = function (dictMonad) {
      return new Control_Apply.Apply(function () {
          return functorStateT(((dictMonad.Bind1()).Apply0()).Functor0());
      }, Control_Monad.ap(monadStateT(dictMonad)));
  };
  var applicativeStateT = function (dictMonad) {
      return new Control_Applicative.Applicative(function () {
          return applyStateT(dictMonad);
      }, function (a) {
          return function (s) {
              return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(a, s));
          };
      });
  };
  var monadStateStateT = function (dictMonad) {
      return new Control_Monad_State_Class.MonadState(function () {
          return monadStateT(dictMonad);
      }, function (f) {
          return StateT((function () {
              var $112 = Control_Applicative.pure(dictMonad.Applicative0());
              return function ($113) {
                  return $112(f($113));
              };
          })());
      });
  };
  exports["StateT"] = StateT;
  exports["runStateT"] = runStateT;
  exports["evalStateT"] = evalStateT;
  exports["functorStateT"] = functorStateT;
  exports["monadStateT"] = monadStateT;
  exports["lazyStateT"] = lazyStateT;
  exports["monadStateStateT"] = monadStateStateT;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Control.Plus"] = $PS["Control.Plus"] || {};
  var exports = $PS["Control.Plus"];                   
  var Plus = function (Alt0, empty) {
      this.Alt0 = Alt0;
      this.empty = empty;
  };       
  var empty = function (dict) {
      return dict.empty;
  };
  exports["Plus"] = Plus;
  exports["empty"] = empty;
})(PS);
(function(exports) {
  "use strict";                                                                                      

  exports.fromFoldableImpl = (function () {
    function Cons(head, tail) {
      this.head = head;
      this.tail = tail;
    }
    var emptyList = {};

    function curryCons(head) {
      return function (tail) {
        return new Cons(head, tail);
      };
    }

    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }

    return function (foldr) {
      return function (xs) {
        return listToArray(foldr(curryCons)(emptyList)(xs));
      };
    };
  })();

  //------------------------------------------------------------------------------
  // Array size ------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.length = function (xs) {
    return xs.length;
  };

  //------------------------------------------------------------------------------
  // Extending arrays ------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.cons = function (e) {
    return function (l) {
      return [e].concat(l);
    };
  };

  //------------------------------------------------------------------------------
  // Non-indexed reads -----------------------------------------------------------
  //------------------------------------------------------------------------------

  exports["uncons'"] = function (empty) {
    return function (next) {
      return function (xs) {
        return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
      };
    };
  };

  exports.findIndexImpl = function (just) {
    return function (nothing) {
      return function (f) {
        return function (xs) {
          for (var i = 0, l = xs.length; i < l; i++) {
            if (f(xs[i])) return just(i);
          }
          return nothing;
        };
      };
    };
  };

  //------------------------------------------------------------------------------
  // Sorting ---------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.sortImpl = function (f) {
    return function (l) {
      return l.slice().sort(function (x, y) {
        return f(x)(y);
      });
    };
  };

  //------------------------------------------------------------------------------
  // Zipping ---------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.zipWith = function (f) {
    return function (xs) {
      return function (ys) {
        var l = xs.length < ys.length ? xs.length : ys.length;
        var result = new Array(l);
        for (var i = 0; i < l; i++) {
          result[i] = f(xs[i])(ys[i]);
        }
        return result;
      };
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
  "use strict";

  var refEq = function (r1) {
    return function (r2) {
      return r1 === r2;
    };
  };

  exports.eqBooleanImpl = refEq;
  exports.eqIntImpl = refEq;
  exports.eqNumberImpl = refEq;
  exports.eqCharImpl = refEq;
  exports.eqStringImpl = refEq;
})(PS["Data.Eq"] = PS["Data.Eq"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Eq"] = $PS["Data.Eq"] || {};
  var exports = $PS["Data.Eq"];
  var $foreign = $PS["Data.Eq"];
  var Eq1 = function (eq1) {
      this.eq1 = eq1;
  };
  var Eq = function (eq) {
      this.eq = eq;
  }; 
  var eqString = new Eq($foreign.eqStringImpl);
  var eqNumber = new Eq($foreign.eqNumberImpl);
  var eqInt = new Eq($foreign.eqIntImpl);
  var eqChar = new Eq($foreign.eqCharImpl);
  var eqBoolean = new Eq($foreign.eqBooleanImpl);
  var eq1 = function (dict) {
      return dict.eq1;
  };
  var eq = function (dict) {
      return dict.eq;
  };
  var notEq = function (dictEq) {
      return function (x) {
          return function (y) {
              return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
          };
      };
  };
  exports["Eq"] = Eq;
  exports["eq"] = eq;
  exports["notEq"] = notEq;
  exports["Eq1"] = Eq1;
  exports["eq1"] = eq1;
  exports["eqBoolean"] = eqBoolean;
  exports["eqInt"] = eqInt;
  exports["eqNumber"] = eqNumber;
  exports["eqChar"] = eqChar;
  exports["eqString"] = eqString;
})(PS);
(function(exports) {
  "use strict";

  exports.foldrArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };

  exports.foldlArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});
(function(exports) {
  "use strict";

  exports.boolConj = function (b1) {
    return function (b2) {
      return b1 && b2;
    };
  };

  exports.boolDisj = function (b1) {
    return function (b2) {
      return b1 || b2;
    };
  };

  exports.boolNot = function (b) {
    return !b;
  };
})(PS["Data.HeytingAlgebra"] = PS["Data.HeytingAlgebra"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.HeytingAlgebra"] = $PS["Data.HeytingAlgebra"] || {};
  var exports = $PS["Data.HeytingAlgebra"];
  var $foreign = $PS["Data.HeytingAlgebra"];
  var HeytingAlgebra = function (conj, disj, ff, implies, not, tt) {
      this.conj = conj;
      this.disj = disj;
      this.ff = ff;
      this.implies = implies;
      this.not = not;
      this.tt = tt;
  };
  var not = function (dict) {
      return dict.not;
  };
  var ff = function (dict) {
      return dict.ff;
  };
  var disj = function (dict) {
      return dict.disj;
  };
  var heytingAlgebraBoolean = new HeytingAlgebra($foreign.boolConj, $foreign.boolDisj, false, function (a) {
      return function (b) {
          return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
  }, $foreign.boolNot, true);
  exports["ff"] = ff;
  exports["disj"] = disj;
  exports["not"] = not;
  exports["heytingAlgebraBoolean"] = heytingAlgebraBoolean;
})(PS);
(function(exports) {
  "use strict";

  exports.concatString = function (s1) {
    return function (s2) {
      return s1 + s2;
    };
  };

  exports.concatArray = function (xs) {
    return function (ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };
})(PS["Data.Semigroup"] = PS["Data.Semigroup"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Semigroup"] = $PS["Data.Semigroup"] || {};
  var exports = $PS["Data.Semigroup"];
  var $foreign = $PS["Data.Semigroup"];
  var Semigroup = function (append) {
      this.append = append;
  }; 
  var semigroupString = new Semigroup($foreign.concatString);
  var semigroupArray = new Semigroup($foreign.concatArray);
  var append = function (dict) {
      return dict.append;
  };
  exports["Semigroup"] = Semigroup;
  exports["append"] = append;
  exports["semigroupString"] = semigroupString;
  exports["semigroupArray"] = semigroupArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Monoid"] = $PS["Data.Monoid"] || {};
  var exports = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Monoid = function (Semigroup0, mempty) {
      this.Semigroup0 = Semigroup0;
      this.mempty = mempty;
  };                 
  var monoidString = new Monoid(function () {
      return Data_Semigroup.semigroupString;
  }, "");                    
  var monoidArray = new Monoid(function () {
      return Data_Semigroup.semigroupArray;
  }, [  ]);
  var mempty = function (dict) {
      return dict.mempty;
  };
  exports["Monoid"] = Monoid;
  exports["mempty"] = mempty;
  exports["monoidString"] = monoidString;
  exports["monoidArray"] = monoidArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Monoid.Disj"] = $PS["Data.Monoid.Disj"] || {};
  var exports = $PS["Data.Monoid.Disj"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];      
  var Disj = function (x) {
      return x;
  };
  var semigroupDisj = function (dictHeytingAlgebra) {
      return new Data_Semigroup.Semigroup(function (v) {
          return function (v1) {
              return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
          };
      });
  };
  var monoidDisj = function (dictHeytingAlgebra) {
      return new Data_Monoid.Monoid(function () {
          return semigroupDisj(dictHeytingAlgebra);
      }, Data_HeytingAlgebra.ff(dictHeytingAlgebra));
  };
  exports["Disj"] = Disj;
  exports["monoidDisj"] = monoidDisj;
})(PS);
(function(exports) {
  "use strict";

  exports.intAdd = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x + y | 0;
    };
  };

  exports.intMul = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x * y | 0;
    };
  };

  exports.numAdd = function (n1) {
    return function (n2) {
      return n1 + n2;
    };
  };

  exports.numMul = function (n1) {
    return function (n2) {
      return n1 * n2;
    };
  };
})(PS["Data.Semiring"] = PS["Data.Semiring"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Semiring"] = $PS["Data.Semiring"] || {};
  var exports = $PS["Data.Semiring"];
  var $foreign = $PS["Data.Semiring"];
  var Semiring = function (add, mul, one, zero) {
      this.add = add;
      this.mul = mul;
      this.one = one;
      this.zero = zero;
  };
  var zero = function (dict) {
      return dict.zero;
  }; 
  var semiringNumber = new Semiring($foreign.numAdd, $foreign.numMul, 1.0, 0.0);
  var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
  var one = function (dict) {
      return dict.one;
  };
  var mul = function (dict) {
      return dict.mul;
  };
  var add = function (dict) {
      return dict.add;
  };
  exports["Semiring"] = Semiring;
  exports["add"] = add;
  exports["zero"] = zero;
  exports["mul"] = mul;
  exports["one"] = one;
  exports["semiringInt"] = semiringInt;
  exports["semiringNumber"] = semiringNumber;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Monoid.Additive"] = $PS["Data.Monoid.Additive"] || {};
  var exports = $PS["Data.Monoid.Additive"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semiring = $PS["Data.Semiring"];        
  var Additive = function (x) {
      return x;
  };
  var semigroupAdditive = function (dictSemiring) {
      return new Data_Semigroup.Semigroup(function (v) {
          return function (v1) {
              return Data_Semiring.add(dictSemiring)(v)(v1);
          };
      });
  };
  var monoidAdditive = function (dictSemiring) {
      return new Data_Monoid.Monoid(function () {
          return semigroupAdditive(dictSemiring);
      }, Data_Semiring.zero(dictSemiring));
  };
  exports["Additive"] = Additive;
  exports["monoidAdditive"] = monoidAdditive;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Newtype"] = $PS["Data.Newtype"] || {};
  var exports = $PS["Data.Newtype"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid_Additive = $PS["Data.Monoid.Additive"];
  var Data_Monoid_Disj = $PS["Data.Monoid.Disj"];                      
  var Newtype = function (unwrap, wrap) {
      this.unwrap = unwrap;
      this.wrap = wrap;
  };
  var wrap = function (dict) {
      return dict.wrap;
  };
  var unwrap = function (dict) {
      return dict.unwrap;
  };
  var un = function (dictNewtype) {
      return function (v) {
          return unwrap(dictNewtype);
      };
  };                        
  var newtypeDisj = new Newtype(function (v) {
      return v;
  }, Data_Monoid_Disj.Disj);
  var newtypeAdditive = new Newtype(function (v) {
      return v;
  }, Data_Monoid_Additive.Additive);
  var alaF = function (dictFunctor) {
      return function (dictFunctor1) {
          return function (dictNewtype) {
              return function (dictNewtype1) {
                  return function (v) {
                      return function (f) {
                          var $96 = Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1));
                          var $97 = Data_Functor.map(dictFunctor)(wrap(dictNewtype));
                          return function ($98) {
                              return $96(f($97($98)));
                          };
                      };
                  };
              };
          };
      };
  };
  exports["unwrap"] = unwrap;
  exports["wrap"] = wrap;
  exports["Newtype"] = Newtype;
  exports["un"] = un;
  exports["alaF"] = alaF;
  exports["newtypeAdditive"] = newtypeAdditive;
  exports["newtypeDisj"] = newtypeDisj;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Foldable"] = $PS["Data.Foldable"] || {};
  var exports = $PS["Data.Foldable"];
  var $foreign = $PS["Data.Foldable"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Monoid_Disj = $PS["Data.Monoid.Disj"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Unit = $PS["Data.Unit"];                
  var Foldable = function (foldMap, foldl, foldr) {
      this.foldMap = foldMap;
      this.foldl = foldl;
      this.foldr = foldr;
  };
  var foldr = function (dict) {
      return dict.foldr;
  };
  var oneOf = function (dictFoldable) {
      return function (dictPlus) {
          return foldr(dictFoldable)(Control_Alt.alt(dictPlus.Alt0()))(Control_Plus.empty(dictPlus));
      };
  };
  var traverse_ = function (dictApplicative) {
      return function (dictFoldable) {
          return function (f) {
              return foldr(dictFoldable)((function () {
                  var $197 = Control_Apply.applySecond(dictApplicative.Apply0());
                  return function ($198) {
                      return $197(f($198));
                  };
              })())(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
          };
      };
  };
  var foldl = function (dict) {
      return dict.foldl;
  };
  var intercalate = function (dictFoldable) {
      return function (dictMonoid) {
          return function (sep) {
              return function (xs) {
                  var go = function (v) {
                      return function (x) {
                          if (v.init) {
                              return {
                                  init: false,
                                  acc: x
                              };
                          };
                          return {
                              init: false,
                              acc: Data_Semigroup.append(dictMonoid.Semigroup0())(v.acc)(Data_Semigroup.append(dictMonoid.Semigroup0())(sep)(x))
                          };
                      };
                  };
                  return (foldl(dictFoldable)(go)({
                      init: true,
                      acc: Data_Monoid.mempty(dictMonoid)
                  })(xs)).acc;
              };
          };
      };
  };
  var product = function (dictFoldable) {
      return function (dictSemiring) {
          return foldl(dictFoldable)(Data_Semiring.mul(dictSemiring))(Data_Semiring.one(dictSemiring));
      };
  };
  var sum = function (dictFoldable) {
      return function (dictSemiring) {
          return foldl(dictFoldable)(Data_Semiring.add(dictSemiring))(Data_Semiring.zero(dictSemiring));
      };
  }; 
  var foldMapDefaultR = function (dictFoldable) {
      return function (dictMonoid) {
          return function (f) {
              return foldr(dictFoldable)(function (x) {
                  return function (acc) {
                      return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
                  };
              })(Data_Monoid.mempty(dictMonoid));
          };
      };
  };
  var foldableArray = new Foldable(function (dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
  }, $foreign.foldlArray, $foreign.foldrArray);
  var foldMap = function (dict) {
      return dict.foldMap;
  };
  var foldM = function (dictFoldable) {
      return function (dictMonad) {
          return function (f) {
              return function (a0) {
                  return foldl(dictFoldable)(function (ma) {
                      return function (b) {
                          return Control_Bind.bind(dictMonad.Bind1())(ma)(Data_Function.flip(f)(b));
                      };
                  })(Control_Applicative.pure(dictMonad.Applicative0())(a0));
              };
          };
      };
  };
  var fold = function (dictFoldable) {
      return function (dictMonoid) {
          return foldMap(dictFoldable)(dictMonoid)(Control_Category.identity(Control_Category.categoryFn));
      };
  };
  var any = function (dictFoldable) {
      return function (dictHeytingAlgebra) {
          return Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Newtype.newtypeDisj)(Data_Newtype.newtypeDisj)(Data_Monoid_Disj.Disj)(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra)));
      };
  };
  var elem = function (dictFoldable) {
      return function (dictEq) {
          var $204 = any(dictFoldable)(Data_HeytingAlgebra.heytingAlgebraBoolean);
          var $205 = Data_Eq.eq(dictEq);
          return function ($206) {
              return $204($205($206));
          };
      };
  };
  var notElem = function (dictFoldable) {
      return function (dictEq) {
          return function (x) {
              var $207 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
              var $208 = elem(dictFoldable)(dictEq)(x);
              return function ($209) {
                  return $207($208($209));
              };
          };
      };
  };
  exports["Foldable"] = Foldable;
  exports["foldr"] = foldr;
  exports["foldl"] = foldl;
  exports["foldMap"] = foldMap;
  exports["fold"] = fold;
  exports["foldM"] = foldM;
  exports["traverse_"] = traverse_;
  exports["oneOf"] = oneOf;
  exports["intercalate"] = intercalate;
  exports["sum"] = sum;
  exports["product"] = product;
  exports["elem"] = elem;
  exports["notElem"] = notElem;
  exports["foldableArray"] = foldableArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];          
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var maybe = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Nothing) {
                  return v;
              };
              if (v2 instanceof Just) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Maybe (line 217, column 1 - line 217, column 51): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  var isNothing = maybe(true)(Data_Function["const"](false));
  var isJust = maybe(false)(Data_Function["const"](true));
  var functorMaybe = new Data_Functor.Functor(function (v) {
      return function (v1) {
          if (v1 instanceof Just) {
              return new Just(v(v1.value0));
          };
          return Nothing.value;
      };
  });
  var fromMaybe = function (a) {
      return maybe(a)(Control_Category.identity(Control_Category.categoryFn));
  };
  var fromJust = function (dictPartial) {
      return function (v) {
          if (v instanceof Just) {
              return v.value0;
          };
          throw new Error("Failed pattern match at Data.Maybe (line 268, column 1 - line 268, column 46): " + [ v.constructor.name ]);
      };
  };
  var applyMaybe = new Control_Apply.Apply(function () {
      return functorMaybe;
  }, function (v) {
      return function (v1) {
          if (v instanceof Just) {
              return Data_Functor.map(functorMaybe)(v.value0)(v1);
          };
          if (v instanceof Nothing) {
              return Nothing.value;
          };
          throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var bindMaybe = new Control_Bind.Bind(function () {
      return applyMaybe;
  }, function (v) {
      return function (v1) {
          if (v instanceof Just) {
              return v1(v.value0);
          };
          if (v instanceof Nothing) {
              return Nothing.value;
          };
          throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var applicativeMaybe = new Control_Applicative.Applicative(function () {
      return applyMaybe;
  }, Just.create);
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["maybe"] = maybe;
  exports["fromMaybe"] = fromMaybe;
  exports["isJust"] = isJust;
  exports["isNothing"] = isNothing;
  exports["fromJust"] = fromJust;
  exports["functorMaybe"] = functorMaybe;
  exports["applicativeMaybe"] = applicativeMaybe;
  exports["bindMaybe"] = bindMaybe;
})(PS);
(function(exports) {
  "use strict";

  var unsafeCompareImpl = function (lt) {
    return function (eq) {
      return function (gt) {
        return function (x) {
          return function (y) {
            return x < y ? lt : x === y ? eq : gt;
          };
        };
      };
    };
  };

  exports.ordBooleanImpl = unsafeCompareImpl;
  exports.ordIntImpl = unsafeCompareImpl;
  exports.ordNumberImpl = unsafeCompareImpl;
  exports.ordStringImpl = unsafeCompareImpl;
  exports.ordCharImpl = unsafeCompareImpl;
})(PS["Data.Ord"] = PS["Data.Ord"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Ordering"] = $PS["Data.Ordering"] || {};
  var exports = $PS["Data.Ordering"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Semigroup = $PS["Data.Semigroup"];      
  var LT = (function () {
      function LT() {

      };
      LT.value = new LT();
      return LT;
  })();
  var GT = (function () {
      function GT() {

      };
      GT.value = new GT();
      return GT;
  })();
  var EQ = (function () {
      function EQ() {

      };
      EQ.value = new EQ();
      return EQ;
  })();
  var semigroupOrdering = new Data_Semigroup.Semigroup(function (v) {
      return function (v1) {
          if (v instanceof LT) {
              return LT.value;
          };
          if (v instanceof GT) {
              return GT.value;
          };
          if (v instanceof EQ) {
              return v1;
          };
          throw new Error("Failed pattern match at Data.Ordering (line 21, column 1 - line 24, column 18): " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var eqOrdering = new Data_Eq.Eq(function (v) {
      return function (v1) {
          if (v instanceof LT && v1 instanceof LT) {
              return true;
          };
          if (v instanceof GT && v1 instanceof GT) {
              return true;
          };
          if (v instanceof EQ && v1 instanceof EQ) {
              return true;
          };
          return false;
      };
  });
  exports["LT"] = LT;
  exports["GT"] = GT;
  exports["EQ"] = EQ;
  exports["eqOrdering"] = eqOrdering;
  exports["semigroupOrdering"] = semigroupOrdering;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Ord"] = $PS["Data.Ord"] || {};
  var exports = $PS["Data.Ord"];
  var $foreign = $PS["Data.Ord"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Ord = function (Eq0, compare) {
      this.Eq0 = Eq0;
      this.compare = compare;
  }; 
  var ordString = new Ord(function () {
      return Data_Eq.eqString;
  }, $foreign.ordStringImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var ordNumber = new Ord(function () {
      return Data_Eq.eqNumber;
  }, $foreign.ordNumberImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var ordInt = new Ord(function () {
      return Data_Eq.eqInt;
  }, $foreign.ordIntImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var ordChar = new Ord(function () {
      return Data_Eq.eqChar;
  }, $foreign.ordCharImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var ordBoolean = new Ord(function () {
      return Data_Eq.eqBoolean;
  }, $foreign.ordBooleanImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var compare = function (dict) {
      return dict.compare;
  };
  var comparing = function (dictOrd) {
      return function (f) {
          return function (x) {
              return function (y) {
                  return compare(dictOrd)(f(x))(f(y));
              };
          };
      };
  };
  var lessThan = function (dictOrd) {
      return function (a1) {
          return function (a2) {
              var v = compare(dictOrd)(a1)(a2);
              if (v instanceof Data_Ordering.LT) {
                  return true;
              };
              return false;
          };
      };
  };
  exports["Ord"] = Ord;
  exports["compare"] = compare;
  exports["lessThan"] = lessThan;
  exports["comparing"] = comparing;
  exports["ordBoolean"] = ordBoolean;
  exports["ordInt"] = ordInt;
  exports["ordNumber"] = ordNumber;
  exports["ordString"] = ordString;
  exports["ordChar"] = ordChar;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Array"] = $PS["Data.Array"] || {};
  var exports = $PS["Data.Array"];
  var $foreign = $PS["Data.Array"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Tuple = $PS["Data.Tuple"];
  var zip = $foreign.zipWith(Data_Tuple.Tuple.create);
  var uncons = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
      return function (xs) {
          return new Data_Maybe.Just({
              head: x,
              tail: xs
          });
      };
  });
  var sortBy = function (comp) {
      return function (xs) {
          var comp$prime = function (x) {
              return function (y) {
                  var v = comp(x)(y);
                  if (v instanceof Data_Ordering.GT) {
                      return 1;
                  };
                  if (v instanceof Data_Ordering.EQ) {
                      return 0;
                  };
                  if (v instanceof Data_Ordering.LT) {
                      return -1 | 0;
                  };
                  throw new Error("Failed pattern match at Data.Array (line 702, column 15 - line 705, column 13): " + [ v.constructor.name ]);
              };
          };
          return $foreign.sortImpl(comp$prime)(xs);
      };
  };
  var sort = function (dictOrd) {
      return function (xs) {
          return sortBy(Data_Ord.compare(dictOrd))(xs);
      };
  };
  var singleton = function (a) {
      return [ a ];
  };
  var some = function (dictAlternative) {
      return function (dictLazy) {
          return function (v) {
              return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())($foreign.cons)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                  return many(dictAlternative)(dictLazy)(v);
              }));
          };
      };
  };
  var many = function (dictAlternative) {
      return function (dictLazy) {
          return function (v) {
              return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())([  ]));
          };
      };
  };
  var fromFoldable = function (dictFoldable) {
      return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
  };
  var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var elemIndex = function (dictEq) {
      return function (x) {
          return findIndex(function (v) {
              return Data_Eq.eq(dictEq)(v)(x);
          });
      };
  };
  exports["fromFoldable"] = fromFoldable;
  exports["singleton"] = singleton;
  exports["some"] = some;
  exports["many"] = many;
  exports["uncons"] = uncons;
  exports["elemIndex"] = elemIndex;
  exports["sort"] = sort;
  exports["zip"] = zip;
  exports["length"] = $foreign.length;
  exports["cons"] = $foreign.cons;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Boolean"] = $PS["Data.Boolean"] || {};
  var exports = $PS["Data.Boolean"];
  var otherwise = true;
  exports["otherwise"] = otherwise;
})(PS);
(function(exports) {
  "use strict";

  exports.topInt = 2147483647;
  exports.bottomInt = -2147483648;

  exports.topChar = String.fromCharCode(65535);
  exports.bottomChar = String.fromCharCode(0);
})(PS["Data.Bounded"] = PS["Data.Bounded"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Bounded"] = $PS["Data.Bounded"] || {};
  var exports = $PS["Data.Bounded"];
  var $foreign = $PS["Data.Bounded"];
  var Data_Ord = $PS["Data.Ord"];                  
  var Bounded = function (Ord0, bottom, top) {
      this.Ord0 = Ord0;
      this.bottom = bottom;
      this.top = top;
  };
  var top = function (dict) {
      return dict.top;
  };                                            
  var boundedInt = new Bounded(function () {
      return Data_Ord.ordInt;
  }, $foreign.bottomInt, $foreign.topInt);
  var boundedChar = new Bounded(function () {
      return Data_Ord.ordChar;
  }, $foreign.bottomChar, $foreign.topChar);
  var bottom = function (dict) {
      return dict.bottom;
  };
  exports["bottom"] = bottom;
  exports["top"] = top;
  exports["boundedInt"] = boundedInt;
  exports["boundedChar"] = boundedChar;
})(PS);
(function(exports) {
  "use strict";

  exports.toCharCode = function (c) {
    return c.charCodeAt(0);
  };

  exports.fromCharCode = function (c) {
    return String.fromCharCode(c);
  };
})(PS["Data.Enum"] = PS["Data.Enum"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Enum"] = $PS["Data.Enum"] || {};
  var exports = $PS["Data.Enum"];
  var $foreign = $PS["Data.Enum"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Enum = function (Ord0, pred, succ) {
      this.Ord0 = Ord0;
      this.pred = pred;
      this.succ = succ;
  };
  var BoundedEnum = function (Bounded0, Enum1, cardinality, fromEnum, toEnum) {
      this.Bounded0 = Bounded0;
      this.Enum1 = Enum1;
      this.cardinality = cardinality;
      this.fromEnum = fromEnum;
      this.toEnum = toEnum;
  };
  var toEnum = function (dict) {
      return dict.toEnum;
  };              
  var fromEnum = function (dict) {
      return dict.fromEnum;
  };
  var toEnumWithDefaults = function (dictBoundedEnum) {
      return function (low) {
          return function (high) {
              return function (x) {
                  var v = toEnum(dictBoundedEnum)(x);
                  if (v instanceof Data_Maybe.Just) {
                      return v.value0;
                  };
                  if (v instanceof Data_Maybe.Nothing) {
                      var $54 = x < fromEnum(dictBoundedEnum)(Data_Bounded.bottom(dictBoundedEnum.Bounded0()));
                      if ($54) {
                          return low;
                      };
                      return high;
                  };
                  throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [ v.constructor.name ]);
              };
          };
      };
  };
  var defaultSucc = function (toEnum$prime) {
      return function (fromEnum$prime) {
          return function (a) {
              return toEnum$prime(fromEnum$prime(a) + 1 | 0);
          };
      };
  };
  var defaultPred = function (toEnum$prime) {
      return function (fromEnum$prime) {
          return function (a) {
              return toEnum$prime(fromEnum$prime(a) - 1 | 0);
          };
      };
  };
  var charToEnum = function (v) {
      if (v >= Data_Bounded.bottom(Data_Bounded.boundedInt) && v <= Data_Bounded.top(Data_Bounded.boundedInt)) {
          return new Data_Maybe.Just($foreign.fromCharCode(v));
      };
      return Data_Maybe.Nothing.value;
  };
  var enumChar = new Enum(function () {
      return Data_Ord.ordChar;
  }, defaultPred(charToEnum)($foreign.toCharCode), defaultSucc(charToEnum)($foreign.toCharCode));
  var boundedEnumChar = new BoundedEnum(function () {
      return Data_Bounded.boundedChar;
  }, function () {
      return enumChar;
  }, $foreign.toCharCode(Data_Bounded.top(Data_Bounded.boundedChar)) - $foreign.toCharCode(Data_Bounded.bottom(Data_Bounded.boundedChar)) | 0, $foreign.toCharCode, charToEnum);
  exports["toEnum"] = toEnum;
  exports["fromEnum"] = fromEnum;
  exports["toEnumWithDefaults"] = toEnumWithDefaults;
  exports["boundedEnumChar"] = boundedEnumChar;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Char"] = $PS["Data.Char"] || {};
  var exports = $PS["Data.Char"];
  var Data_Enum = $PS["Data.Enum"];                
  var toCharCode = Data_Enum.fromEnum(Data_Enum.boundedEnumChar);
  var fromCharCode = Data_Enum.toEnum(Data_Enum.boundedEnumChar);
  exports["toCharCode"] = toCharCode;
  exports["fromCharCode"] = fromCharCode;
})(PS);
(function(exports) {
  exports.withCharCode = function(f) {
      return function (c) {
          return String.fromCharCode(f(c.charCodeAt()));
      }
  }
})(PS["Data.Char.Unicode"] = PS["Data.Char.Unicode"] || {});
(function(exports) {
  "use strict";

  exports.fromNumberImpl = function (just) {
    return function (nothing) {
      return function (n) {
        /* jshint bitwise: false */
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };

  exports.toNumber = function (n) {
    return n;
  };
})(PS["Data.Int"] = PS["Data.Int"] || {});
(function(exports) {
  /* globals exports */
  "use strict";         

  exports.infinity = Infinity;
})(PS["Global"] = PS["Global"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Global"] = $PS["Global"] || {};
  var exports = $PS["Global"];
  var $foreign = $PS["Global"];
  exports["infinity"] = $foreign.infinity;
})(PS);
(function(exports) {
  "use strict";          

  exports.floor = Math.floor;

  exports.pow = function (n) {
    return function (p) {
      return Math.pow(n, p);
    };
  };

  exports.round = Math.round;  

  exports.pi = Math.PI;
})(PS["Math"] = PS["Math"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Math"] = $PS["Math"] || {};
  var exports = $PS["Math"];
  var $foreign = $PS["Math"];
  exports["floor"] = $foreign.floor;
  exports["pow"] = $foreign.pow;
  exports["round"] = $foreign.round;
  exports["pi"] = $foreign.pi;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Int"] = $PS["Data.Int"] || {};
  var exports = $PS["Data.Int"];
  var $foreign = $PS["Data.Int"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Global = $PS["Global"];
  var $$Math = $PS["Math"];         
  var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var unsafeClamp = function (x) {
      if (x === Global.infinity) {
          return 0;
      };
      if (x === -Global.infinity) {
          return 0;
      };
      if (x >= $foreign.toNumber(Data_Bounded.top(Data_Bounded.boundedInt))) {
          return Data_Bounded.top(Data_Bounded.boundedInt);
      };
      if (x <= $foreign.toNumber(Data_Bounded.bottom(Data_Bounded.boundedInt))) {
          return Data_Bounded.bottom(Data_Bounded.boundedInt);
      };
      if (Data_Boolean.otherwise) {
          return Data_Maybe.fromMaybe(0)(fromNumber(x));
      };
      throw new Error("Failed pattern match at Data.Int (line 66, column 1 - line 66, column 29): " + [ x.constructor.name ]);
  };
  var round = function ($23) {
      return unsafeClamp($$Math.round($23));
  };
  var floor = function ($24) {
      return unsafeClamp($$Math.floor($24));
  };
  exports["floor"] = floor;
  exports["round"] = round;
  exports["toNumber"] = $foreign.toNumber;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Char.Unicode.Internal"] = $PS["Data.Char.Unicode.Internal"] || {};
  var exports = $PS["Data.Char.Unicode.Internal"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ordering = $PS["Data.Ordering"];        
  var NUMCAT_LU = (function () {
      function NUMCAT_LU() {

      };
      NUMCAT_LU.value = new NUMCAT_LU();
      return NUMCAT_LU;
  })();
  var NUMCAT_LL = (function () {
      function NUMCAT_LL() {

      };
      NUMCAT_LL.value = new NUMCAT_LL();
      return NUMCAT_LL;
  })();
  var NUMCAT_LT = (function () {
      function NUMCAT_LT() {

      };
      NUMCAT_LT.value = new NUMCAT_LT();
      return NUMCAT_LT;
  })();
  var NUMCAT_LM = (function () {
      function NUMCAT_LM() {

      };
      NUMCAT_LM.value = new NUMCAT_LM();
      return NUMCAT_LM;
  })();
  var NUMCAT_LO = (function () {
      function NUMCAT_LO() {

      };
      NUMCAT_LO.value = new NUMCAT_LO();
      return NUMCAT_LO;
  })();
  var NUMCAT_MN = (function () {
      function NUMCAT_MN() {

      };
      NUMCAT_MN.value = new NUMCAT_MN();
      return NUMCAT_MN;
  })();
  var NUMCAT_MC = (function () {
      function NUMCAT_MC() {

      };
      NUMCAT_MC.value = new NUMCAT_MC();
      return NUMCAT_MC;
  })();
  var NUMCAT_ME = (function () {
      function NUMCAT_ME() {

      };
      NUMCAT_ME.value = new NUMCAT_ME();
      return NUMCAT_ME;
  })();
  var NUMCAT_ND = (function () {
      function NUMCAT_ND() {

      };
      NUMCAT_ND.value = new NUMCAT_ND();
      return NUMCAT_ND;
  })();
  var NUMCAT_NL = (function () {
      function NUMCAT_NL() {

      };
      NUMCAT_NL.value = new NUMCAT_NL();
      return NUMCAT_NL;
  })();
  var NUMCAT_NO = (function () {
      function NUMCAT_NO() {

      };
      NUMCAT_NO.value = new NUMCAT_NO();
      return NUMCAT_NO;
  })();
  var NUMCAT_PC = (function () {
      function NUMCAT_PC() {

      };
      NUMCAT_PC.value = new NUMCAT_PC();
      return NUMCAT_PC;
  })();
  var NUMCAT_PD = (function () {
      function NUMCAT_PD() {

      };
      NUMCAT_PD.value = new NUMCAT_PD();
      return NUMCAT_PD;
  })();
  var NUMCAT_PS = (function () {
      function NUMCAT_PS() {

      };
      NUMCAT_PS.value = new NUMCAT_PS();
      return NUMCAT_PS;
  })();
  var NUMCAT_PE = (function () {
      function NUMCAT_PE() {

      };
      NUMCAT_PE.value = new NUMCAT_PE();
      return NUMCAT_PE;
  })();
  var NUMCAT_PI = (function () {
      function NUMCAT_PI() {

      };
      NUMCAT_PI.value = new NUMCAT_PI();
      return NUMCAT_PI;
  })();
  var NUMCAT_PF = (function () {
      function NUMCAT_PF() {

      };
      NUMCAT_PF.value = new NUMCAT_PF();
      return NUMCAT_PF;
  })();
  var NUMCAT_PO = (function () {
      function NUMCAT_PO() {

      };
      NUMCAT_PO.value = new NUMCAT_PO();
      return NUMCAT_PO;
  })();
  var NUMCAT_SM = (function () {
      function NUMCAT_SM() {

      };
      NUMCAT_SM.value = new NUMCAT_SM();
      return NUMCAT_SM;
  })();
  var NUMCAT_SC = (function () {
      function NUMCAT_SC() {

      };
      NUMCAT_SC.value = new NUMCAT_SC();
      return NUMCAT_SC;
  })();
  var NUMCAT_SK = (function () {
      function NUMCAT_SK() {

      };
      NUMCAT_SK.value = new NUMCAT_SK();
      return NUMCAT_SK;
  })();
  var NUMCAT_SO = (function () {
      function NUMCAT_SO() {

      };
      NUMCAT_SO.value = new NUMCAT_SO();
      return NUMCAT_SO;
  })();
  var NUMCAT_ZS = (function () {
      function NUMCAT_ZS() {

      };
      NUMCAT_ZS.value = new NUMCAT_ZS();
      return NUMCAT_ZS;
  })();
  var NUMCAT_ZL = (function () {
      function NUMCAT_ZL() {

      };
      NUMCAT_ZL.value = new NUMCAT_ZL();
      return NUMCAT_ZL;
  })();
  var NUMCAT_ZP = (function () {
      function NUMCAT_ZP() {

      };
      NUMCAT_ZP.value = new NUMCAT_ZP();
      return NUMCAT_ZP;
  })();
  var NUMCAT_CC = (function () {
      function NUMCAT_CC() {

      };
      NUMCAT_CC.value = new NUMCAT_CC();
      return NUMCAT_CC;
  })();
  var NUMCAT_CF = (function () {
      function NUMCAT_CF() {

      };
      NUMCAT_CF.value = new NUMCAT_CF();
      return NUMCAT_CF;
  })();
  var NUMCAT_CS = (function () {
      function NUMCAT_CS() {

      };
      NUMCAT_CS.value = new NUMCAT_CS();
      return NUMCAT_CS;
  })();
  var NUMCAT_CO = (function () {
      function NUMCAT_CO() {

      };
      NUMCAT_CO.value = new NUMCAT_CO();
      return NUMCAT_CO;
  })();
  var NUMCAT_CN = (function () {
      function NUMCAT_CN() {

      };
      NUMCAT_CN.value = new NUMCAT_CN();
      return NUMCAT_CN;
  })();
  var numSpaceBlocks = 8;
  var numLat1Blocks = 63;
  var numConvBlocks = 1230;
  var numBlocks = 2783;    
  var gencatZS = 2;
  var rule1 = {
      category: gencatZS,
      unicodeCat: NUMCAT_ZS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var spacechars = [ {
      start: 32,
      length: 1,
      convRule: rule1
  }, {
      start: 160,
      length: 1,
      convRule: rule1
  }, {
      start: 5760,
      length: 1,
      convRule: rule1
  }, {
      start: 6158,
      length: 1,
      convRule: rule1
  }, {
      start: 8192,
      length: 11,
      convRule: rule1
  }, {
      start: 8239,
      length: 1,
      convRule: rule1
  }, {
      start: 8287,
      length: 1,
      convRule: rule1
  }, {
      start: 12288,
      length: 1,
      convRule: rule1
  } ];
  var gencatZP = 67108864;
  var rule140 = {
      category: gencatZP,
      unicodeCat: NUMCAT_ZP.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatZL = 33554432;
  var rule139 = {
      category: gencatZL,
      unicodeCat: NUMCAT_ZL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatSO = 8192;
  var rule13 = {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var rule148 = {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: 0,
      lowdist: 26,
      titledist: 0
  };
  var rule149 = {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: -26 | 0,
      lowdist: 0,
      titledist: -26 | 0
  };
  var gencatSM = 64;
  var rule6 = {
      category: gencatSM,
      unicodeCat: NUMCAT_SM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatSK = 1024;
  var rule10 = {
      category: gencatSK,
      unicodeCat: NUMCAT_SK.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatSC = 8;
  var rule3 = {
      category: gencatSC,
      unicodeCat: NUMCAT_SC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatPS = 16;
  var rule4 = {
      category: gencatPS,
      unicodeCat: NUMCAT_PS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatPO = 4;
  var rule2 = {
      category: gencatPO,
      unicodeCat: NUMCAT_PO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatPI = 16384;
  var rule15 = {
      category: gencatPI,
      unicodeCat: NUMCAT_PI.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatPF = 131072;
  var rule19 = {
      category: gencatPF,
      unicodeCat: NUMCAT_PF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatPE = 32;
  var rule5 = {
      category: gencatPE,
      unicodeCat: NUMCAT_PE.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatPD = 128;
  var rule7 = {
      category: gencatPD,
      unicodeCat: NUMCAT_PD.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatPC = 2048;
  var rule11 = {
      category: gencatPC,
      unicodeCat: NUMCAT_PC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatNO = 65536;
  var rule17 = {
      category: gencatNO,
      unicodeCat: NUMCAT_NO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatNL = 16777216;
  var rule116 = {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var rule146 = {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: 0,
      lowdist: 16,
      titledist: 0
  };
  var rule147 = {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: -16 | 0,
      lowdist: 0,
      titledist: -16 | 0
  };
  var gencatND = 256;
  var rule8 = {
      category: gencatND,
      unicodeCat: NUMCAT_ND.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatMN = 2097152;
  var rule84 = {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var rule85 = {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 1,
      updist: 84,
      lowdist: 0,
      titledist: 84
  };
  var gencatME = 4194304;
  var rule109 = {
      category: gencatME,
      unicodeCat: NUMCAT_ME.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatMC = 8388608;
  var rule114 = {
      category: gencatMC,
      unicodeCat: NUMCAT_MC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatLU = 512;
  var nullrule = {
      category: gencatLU,
      unicodeCat: NUMCAT_CN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var rule105 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -60 | 0,
      titledist: 0
  };
  var rule107 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7 | 0,
      titledist: 0
  };
  var rule108 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 80,
      titledist: 0
  };
  var rule110 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 15,
      titledist: 0
  };
  var rule112 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 48,
      titledist: 0
  };
  var rule115 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 7264,
      titledist: 0
  };
  var rule120 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7615 | 0,
      titledist: 0
  };
  var rule122 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
  };
  var rule131 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -74 | 0,
      titledist: 0
  };
  var rule134 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -86 | 0,
      titledist: 0
  };
  var rule135 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -100 | 0,
      titledist: 0
  };
  var rule136 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -112 | 0,
      titledist: 0
  };
  var rule137 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -128 | 0,
      titledist: 0
  };
  var rule138 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -126 | 0,
      titledist: 0
  };
  var rule141 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7517 | 0,
      titledist: 0
  };
  var rule142 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8383 | 0,
      titledist: 0
  };
  var rule143 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8262 | 0,
      titledist: 0
  };
  var rule144 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 28,
      titledist: 0
  };
  var rule150 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10743 | 0,
      titledist: 0
  };
  var rule151 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3814 | 0,
      titledist: 0
  };
  var rule152 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10727 | 0,
      titledist: 0
  };
  var rule155 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10780 | 0,
      titledist: 0
  };
  var rule156 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10749 | 0,
      titledist: 0
  };
  var rule157 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10783 | 0,
      titledist: 0
  };
  var rule158 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10782 | 0,
      titledist: 0
  };
  var rule159 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10815 | 0,
      titledist: 0
  };
  var rule161 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35332 | 0,
      titledist: 0
  };
  var rule162 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42280 | 0,
      titledist: 0
  };
  var rule165 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 40,
      titledist: 0
  };
  var rule21 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 1,
      titledist: 0
  };
  var rule23 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -199 | 0,
      titledist: 0
  };
  var rule25 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -121 | 0,
      titledist: 0
  };
  var rule28 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 210,
      titledist: 0
  };
  var rule29 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 206,
      titledist: 0
  };
  var rule30 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 205,
      titledist: 0
  };
  var rule31 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 79,
      titledist: 0
  };
  var rule32 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 202,
      titledist: 0
  };
  var rule33 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 203,
      titledist: 0
  };
  var rule34 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 207,
      titledist: 0
  };
  var rule36 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 211,
      titledist: 0
  };
  var rule37 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 209,
      titledist: 0
  };
  var rule39 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 213,
      titledist: 0
  };
  var rule41 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 214,
      titledist: 0
  };
  var rule42 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 218,
      titledist: 0
  };
  var rule43 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 217,
      titledist: 0
  };
  var rule44 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 219,
      titledist: 0
  };
  var rule47 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 2,
      titledist: 1
  };
  var rule51 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -97 | 0,
      titledist: 0
  };
  var rule52 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -56 | 0,
      titledist: 0
  };
  var rule53 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -130 | 0,
      titledist: 0
  };
  var rule54 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10795,
      titledist: 0
  };
  var rule55 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -163 | 0,
      titledist: 0
  };
  var rule56 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10792,
      titledist: 0
  };
  var rule58 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -195 | 0,
      titledist: 0
  };
  var rule59 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 69,
      titledist: 0
  };
  var rule60 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 71,
      titledist: 0
  };
  var rule86 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38,
      titledist: 0
  };
  var rule87 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 37,
      titledist: 0
  };
  var rule88 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 64,
      titledist: 0
  };
  var rule89 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 63,
      titledist: 0
  };
  var rule9 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 32,
      titledist: 0
  };
  var rule95 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 8,
      titledist: 0
  };
  var rule98 = {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatLT = 524288;
  var rule129 = {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
  };
  var rule132 = {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -9 | 0,
      titledist: 0
  };
  var rule48 = {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 1,
      titledist: 0
  };
  var gencatLO = 262144;
  var rule45 = {
      category: gencatLO,
      unicodeCat: NUMCAT_LO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatLM = 1048576;
  var rule83 = {
      category: gencatLM,
      unicodeCat: NUMCAT_LM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatLL = 4096;
  var rule100 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -54 | 0,
      lowdist: 0,
      titledist: -54 | 0
  };
  var rule101 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -8 | 0,
      lowdist: 0,
      titledist: -8 | 0
  };
  var rule102 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -86 | 0,
      lowdist: 0,
      titledist: -86 | 0
  };
  var rule103 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -80 | 0,
      lowdist: 0,
      titledist: -80 | 0
  };
  var rule104 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 7,
      lowdist: 0,
      titledist: 7
  };
  var rule106 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -96 | 0,
      lowdist: 0,
      titledist: -96 | 0
  };
  var rule111 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -15 | 0,
      lowdist: 0,
      titledist: -15 | 0
  };
  var rule113 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -48 | 0,
      lowdist: 0,
      titledist: -48 | 0
  };
  var rule117 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35332,
      lowdist: 0,
      titledist: 35332
  };
  var rule118 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3814,
      lowdist: 0,
      titledist: 3814
  };
  var rule119 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -59 | 0,
      lowdist: 0,
      titledist: -59 | 0
  };
  var rule12 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -32 | 0,
      lowdist: 0,
      titledist: -32 | 0
  };
  var rule121 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 8,
      lowdist: 0,
      titledist: 8
  };
  var rule123 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 74,
      lowdist: 0,
      titledist: 74
  };
  var rule124 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 86,
      lowdist: 0,
      titledist: 86
  };
  var rule125 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 100,
      lowdist: 0,
      titledist: 100
  };
  var rule126 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 128,
      lowdist: 0,
      titledist: 128
  };
  var rule127 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 112,
      lowdist: 0,
      titledist: 112
  };
  var rule128 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 126,
      lowdist: 0,
      titledist: 126
  };
  var rule130 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 9,
      lowdist: 0,
      titledist: 9
  };
  var rule133 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7205 | 0,
      lowdist: 0,
      titledist: -7205 | 0
  };
  var rule14 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var rule145 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -28 | 0,
      lowdist: 0,
      titledist: -28 | 0
  };
  var rule153 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10795 | 0,
      lowdist: 0,
      titledist: -10795 | 0
  };
  var rule154 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10792 | 0,
      lowdist: 0,
      titledist: -10792 | 0
  };
  var rule160 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7264 | 0,
      lowdist: 0,
      titledist: -7264 | 0
  };
  var rule166 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -40 | 0,
      lowdist: 0,
      titledist: -40 | 0
  };
  var rule18 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 743,
      lowdist: 0,
      titledist: 743
  };
  var rule20 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 121,
      lowdist: 0,
      titledist: 121
  };
  var rule22 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 0,
      titledist: -1 | 0
  };
  var rule24 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -232 | 0,
      lowdist: 0,
      titledist: -232 | 0
  };
  var rule26 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -300 | 0,
      lowdist: 0,
      titledist: -300 | 0
  };
  var rule27 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 195,
      lowdist: 0,
      titledist: 195
  };
  var rule35 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 97,
      lowdist: 0,
      titledist: 97
  };
  var rule38 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 163,
      lowdist: 0,
      titledist: 163
  };
  var rule40 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 130,
      lowdist: 0,
      titledist: 130
  };
  var rule46 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 56,
      lowdist: 0,
      titledist: 56
  };
  var rule49 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -2 | 0,
      lowdist: 0,
      titledist: -1 | 0
  };
  var rule50 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -79 | 0,
      lowdist: 0,
      titledist: -79 | 0
  };
  var rule57 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10815,
      lowdist: 0,
      titledist: 10815
  };
  var rule61 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10783,
      lowdist: 0,
      titledist: 10783
  };
  var rule62 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10780,
      lowdist: 0,
      titledist: 10780
  };
  var rule63 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10782,
      lowdist: 0,
      titledist: 10782
  };
  var rule64 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -210 | 0,
      lowdist: 0,
      titledist: -210 | 0
  };
  var rule65 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -206 | 0,
      lowdist: 0,
      titledist: -206 | 0
  };
  var rule66 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -205 | 0,
      lowdist: 0,
      titledist: -205 | 0
  };
  var rule67 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -202 | 0,
      lowdist: 0,
      titledist: -202 | 0
  };
  var rule68 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -203 | 0,
      lowdist: 0,
      titledist: -203 | 0
  };
  var rule69 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -207 | 0,
      lowdist: 0,
      titledist: -207 | 0
  };
  var rule70 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42280,
      lowdist: 0,
      titledist: 42280
  };
  var rule71 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -209 | 0,
      lowdist: 0,
      titledist: -209 | 0
  };
  var rule72 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -211 | 0,
      lowdist: 0,
      titledist: -211 | 0
  };
  var rule73 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10743,
      lowdist: 0,
      titledist: 10743
  };
  var rule74 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10749,
      lowdist: 0,
      titledist: 10749
  };
  var rule75 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -213 | 0,
      lowdist: 0,
      titledist: -213 | 0
  };
  var rule76 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -214 | 0,
      lowdist: 0,
      titledist: -214 | 0
  };
  var rule77 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10727,
      lowdist: 0,
      titledist: 10727
  };
  var rule78 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -218 | 0,
      lowdist: 0,
      titledist: -218 | 0
  };
  var rule79 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -69 | 0,
      lowdist: 0,
      titledist: -69 | 0
  };
  var rule80 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -217 | 0,
      lowdist: 0,
      titledist: -217 | 0
  };
  var rule81 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -71 | 0,
      lowdist: 0,
      titledist: -71 | 0
  };
  var rule82 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -219 | 0,
      lowdist: 0,
      titledist: -219 | 0
  };
  var rule90 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38 | 0,
      lowdist: 0,
      titledist: -38 | 0
  };
  var rule91 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -37 | 0,
      lowdist: 0,
      titledist: -37 | 0
  };
  var rule92 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -31 | 0,
      lowdist: 0,
      titledist: -31 | 0
  };
  var rule93 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -64 | 0,
      lowdist: 0,
      titledist: -64 | 0
  };
  var rule94 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -63 | 0,
      lowdist: 0,
      titledist: -63 | 0
  };
  var rule96 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -62 | 0,
      lowdist: 0,
      titledist: -62 | 0
  };
  var rule97 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -57 | 0,
      lowdist: 0,
      titledist: -57 | 0
  };
  var rule99 = {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -47 | 0,
      lowdist: 0,
      titledist: -47 | 0
  };
  var gencatCS = 134217728;
  var rule163 = {
      category: gencatCS,
      unicodeCat: NUMCAT_CS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatCO = 268435456;
  var rule164 = {
      category: gencatCO,
      unicodeCat: NUMCAT_CO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatCF = 32768;
  var rule16 = {
      category: gencatCF,
      unicodeCat: NUMCAT_CF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var gencatCC = 1;
  var rule0 = {
      category: gencatCC,
      unicodeCat: NUMCAT_CC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
  };
  var convchars = [ {
      start: 65,
      length: 26,
      convRule: rule9
  }, {
      start: 97,
      length: 26,
      convRule: rule12
  }, {
      start: 181,
      length: 1,
      convRule: rule18
  }, {
      start: 192,
      length: 23,
      convRule: rule9
  }, {
      start: 216,
      length: 7,
      convRule: rule9
  }, {
      start: 224,
      length: 23,
      convRule: rule12
  }, {
      start: 248,
      length: 7,
      convRule: rule12
  }, {
      start: 255,
      length: 1,
      convRule: rule20
  }, {
      start: 256,
      length: 1,
      convRule: rule21
  }, {
      start: 257,
      length: 1,
      convRule: rule22
  }, {
      start: 258,
      length: 1,
      convRule: rule21
  }, {
      start: 259,
      length: 1,
      convRule: rule22
  }, {
      start: 260,
      length: 1,
      convRule: rule21
  }, {
      start: 261,
      length: 1,
      convRule: rule22
  }, {
      start: 262,
      length: 1,
      convRule: rule21
  }, {
      start: 263,
      length: 1,
      convRule: rule22
  }, {
      start: 264,
      length: 1,
      convRule: rule21
  }, {
      start: 265,
      length: 1,
      convRule: rule22
  }, {
      start: 266,
      length: 1,
      convRule: rule21
  }, {
      start: 267,
      length: 1,
      convRule: rule22
  }, {
      start: 268,
      length: 1,
      convRule: rule21
  }, {
      start: 269,
      length: 1,
      convRule: rule22
  }, {
      start: 270,
      length: 1,
      convRule: rule21
  }, {
      start: 271,
      length: 1,
      convRule: rule22
  }, {
      start: 272,
      length: 1,
      convRule: rule21
  }, {
      start: 273,
      length: 1,
      convRule: rule22
  }, {
      start: 274,
      length: 1,
      convRule: rule21
  }, {
      start: 275,
      length: 1,
      convRule: rule22
  }, {
      start: 276,
      length: 1,
      convRule: rule21
  }, {
      start: 277,
      length: 1,
      convRule: rule22
  }, {
      start: 278,
      length: 1,
      convRule: rule21
  }, {
      start: 279,
      length: 1,
      convRule: rule22
  }, {
      start: 280,
      length: 1,
      convRule: rule21
  }, {
      start: 281,
      length: 1,
      convRule: rule22
  }, {
      start: 282,
      length: 1,
      convRule: rule21
  }, {
      start: 283,
      length: 1,
      convRule: rule22
  }, {
      start: 284,
      length: 1,
      convRule: rule21
  }, {
      start: 285,
      length: 1,
      convRule: rule22
  }, {
      start: 286,
      length: 1,
      convRule: rule21
  }, {
      start: 287,
      length: 1,
      convRule: rule22
  }, {
      start: 288,
      length: 1,
      convRule: rule21
  }, {
      start: 289,
      length: 1,
      convRule: rule22
  }, {
      start: 290,
      length: 1,
      convRule: rule21
  }, {
      start: 291,
      length: 1,
      convRule: rule22
  }, {
      start: 292,
      length: 1,
      convRule: rule21
  }, {
      start: 293,
      length: 1,
      convRule: rule22
  }, {
      start: 294,
      length: 1,
      convRule: rule21
  }, {
      start: 295,
      length: 1,
      convRule: rule22
  }, {
      start: 296,
      length: 1,
      convRule: rule21
  }, {
      start: 297,
      length: 1,
      convRule: rule22
  }, {
      start: 298,
      length: 1,
      convRule: rule21
  }, {
      start: 299,
      length: 1,
      convRule: rule22
  }, {
      start: 300,
      length: 1,
      convRule: rule21
  }, {
      start: 301,
      length: 1,
      convRule: rule22
  }, {
      start: 302,
      length: 1,
      convRule: rule21
  }, {
      start: 303,
      length: 1,
      convRule: rule22
  }, {
      start: 304,
      length: 1,
      convRule: rule23
  }, {
      start: 305,
      length: 1,
      convRule: rule24
  }, {
      start: 306,
      length: 1,
      convRule: rule21
  }, {
      start: 307,
      length: 1,
      convRule: rule22
  }, {
      start: 308,
      length: 1,
      convRule: rule21
  }, {
      start: 309,
      length: 1,
      convRule: rule22
  }, {
      start: 310,
      length: 1,
      convRule: rule21
  }, {
      start: 311,
      length: 1,
      convRule: rule22
  }, {
      start: 313,
      length: 1,
      convRule: rule21
  }, {
      start: 314,
      length: 1,
      convRule: rule22
  }, {
      start: 315,
      length: 1,
      convRule: rule21
  }, {
      start: 316,
      length: 1,
      convRule: rule22
  }, {
      start: 317,
      length: 1,
      convRule: rule21
  }, {
      start: 318,
      length: 1,
      convRule: rule22
  }, {
      start: 319,
      length: 1,
      convRule: rule21
  }, {
      start: 320,
      length: 1,
      convRule: rule22
  }, {
      start: 321,
      length: 1,
      convRule: rule21
  }, {
      start: 322,
      length: 1,
      convRule: rule22
  }, {
      start: 323,
      length: 1,
      convRule: rule21
  }, {
      start: 324,
      length: 1,
      convRule: rule22
  }, {
      start: 325,
      length: 1,
      convRule: rule21
  }, {
      start: 326,
      length: 1,
      convRule: rule22
  }, {
      start: 327,
      length: 1,
      convRule: rule21
  }, {
      start: 328,
      length: 1,
      convRule: rule22
  }, {
      start: 330,
      length: 1,
      convRule: rule21
  }, {
      start: 331,
      length: 1,
      convRule: rule22
  }, {
      start: 332,
      length: 1,
      convRule: rule21
  }, {
      start: 333,
      length: 1,
      convRule: rule22
  }, {
      start: 334,
      length: 1,
      convRule: rule21
  }, {
      start: 335,
      length: 1,
      convRule: rule22
  }, {
      start: 336,
      length: 1,
      convRule: rule21
  }, {
      start: 337,
      length: 1,
      convRule: rule22
  }, {
      start: 338,
      length: 1,
      convRule: rule21
  }, {
      start: 339,
      length: 1,
      convRule: rule22
  }, {
      start: 340,
      length: 1,
      convRule: rule21
  }, {
      start: 341,
      length: 1,
      convRule: rule22
  }, {
      start: 342,
      length: 1,
      convRule: rule21
  }, {
      start: 343,
      length: 1,
      convRule: rule22
  }, {
      start: 344,
      length: 1,
      convRule: rule21
  }, {
      start: 345,
      length: 1,
      convRule: rule22
  }, {
      start: 346,
      length: 1,
      convRule: rule21
  }, {
      start: 347,
      length: 1,
      convRule: rule22
  }, {
      start: 348,
      length: 1,
      convRule: rule21
  }, {
      start: 349,
      length: 1,
      convRule: rule22
  }, {
      start: 350,
      length: 1,
      convRule: rule21
  }, {
      start: 351,
      length: 1,
      convRule: rule22
  }, {
      start: 352,
      length: 1,
      convRule: rule21
  }, {
      start: 353,
      length: 1,
      convRule: rule22
  }, {
      start: 354,
      length: 1,
      convRule: rule21
  }, {
      start: 355,
      length: 1,
      convRule: rule22
  }, {
      start: 356,
      length: 1,
      convRule: rule21
  }, {
      start: 357,
      length: 1,
      convRule: rule22
  }, {
      start: 358,
      length: 1,
      convRule: rule21
  }, {
      start: 359,
      length: 1,
      convRule: rule22
  }, {
      start: 360,
      length: 1,
      convRule: rule21
  }, {
      start: 361,
      length: 1,
      convRule: rule22
  }, {
      start: 362,
      length: 1,
      convRule: rule21
  }, {
      start: 363,
      length: 1,
      convRule: rule22
  }, {
      start: 364,
      length: 1,
      convRule: rule21
  }, {
      start: 365,
      length: 1,
      convRule: rule22
  }, {
      start: 366,
      length: 1,
      convRule: rule21
  }, {
      start: 367,
      length: 1,
      convRule: rule22
  }, {
      start: 368,
      length: 1,
      convRule: rule21
  }, {
      start: 369,
      length: 1,
      convRule: rule22
  }, {
      start: 370,
      length: 1,
      convRule: rule21
  }, {
      start: 371,
      length: 1,
      convRule: rule22
  }, {
      start: 372,
      length: 1,
      convRule: rule21
  }, {
      start: 373,
      length: 1,
      convRule: rule22
  }, {
      start: 374,
      length: 1,
      convRule: rule21
  }, {
      start: 375,
      length: 1,
      convRule: rule22
  }, {
      start: 376,
      length: 1,
      convRule: rule25
  }, {
      start: 377,
      length: 1,
      convRule: rule21
  }, {
      start: 378,
      length: 1,
      convRule: rule22
  }, {
      start: 379,
      length: 1,
      convRule: rule21
  }, {
      start: 380,
      length: 1,
      convRule: rule22
  }, {
      start: 381,
      length: 1,
      convRule: rule21
  }, {
      start: 382,
      length: 1,
      convRule: rule22
  }, {
      start: 383,
      length: 1,
      convRule: rule26
  }, {
      start: 384,
      length: 1,
      convRule: rule27
  }, {
      start: 385,
      length: 1,
      convRule: rule28
  }, {
      start: 386,
      length: 1,
      convRule: rule21
  }, {
      start: 387,
      length: 1,
      convRule: rule22
  }, {
      start: 388,
      length: 1,
      convRule: rule21
  }, {
      start: 389,
      length: 1,
      convRule: rule22
  }, {
      start: 390,
      length: 1,
      convRule: rule29
  }, {
      start: 391,
      length: 1,
      convRule: rule21
  }, {
      start: 392,
      length: 1,
      convRule: rule22
  }, {
      start: 393,
      length: 2,
      convRule: rule30
  }, {
      start: 395,
      length: 1,
      convRule: rule21
  }, {
      start: 396,
      length: 1,
      convRule: rule22
  }, {
      start: 398,
      length: 1,
      convRule: rule31
  }, {
      start: 399,
      length: 1,
      convRule: rule32
  }, {
      start: 400,
      length: 1,
      convRule: rule33
  }, {
      start: 401,
      length: 1,
      convRule: rule21
  }, {
      start: 402,
      length: 1,
      convRule: rule22
  }, {
      start: 403,
      length: 1,
      convRule: rule30
  }, {
      start: 404,
      length: 1,
      convRule: rule34
  }, {
      start: 405,
      length: 1,
      convRule: rule35
  }, {
      start: 406,
      length: 1,
      convRule: rule36
  }, {
      start: 407,
      length: 1,
      convRule: rule37
  }, {
      start: 408,
      length: 1,
      convRule: rule21
  }, {
      start: 409,
      length: 1,
      convRule: rule22
  }, {
      start: 410,
      length: 1,
      convRule: rule38
  }, {
      start: 412,
      length: 1,
      convRule: rule36
  }, {
      start: 413,
      length: 1,
      convRule: rule39
  }, {
      start: 414,
      length: 1,
      convRule: rule40
  }, {
      start: 415,
      length: 1,
      convRule: rule41
  }, {
      start: 416,
      length: 1,
      convRule: rule21
  }, {
      start: 417,
      length: 1,
      convRule: rule22
  }, {
      start: 418,
      length: 1,
      convRule: rule21
  }, {
      start: 419,
      length: 1,
      convRule: rule22
  }, {
      start: 420,
      length: 1,
      convRule: rule21
  }, {
      start: 421,
      length: 1,
      convRule: rule22
  }, {
      start: 422,
      length: 1,
      convRule: rule42
  }, {
      start: 423,
      length: 1,
      convRule: rule21
  }, {
      start: 424,
      length: 1,
      convRule: rule22
  }, {
      start: 425,
      length: 1,
      convRule: rule42
  }, {
      start: 428,
      length: 1,
      convRule: rule21
  }, {
      start: 429,
      length: 1,
      convRule: rule22
  }, {
      start: 430,
      length: 1,
      convRule: rule42
  }, {
      start: 431,
      length: 1,
      convRule: rule21
  }, {
      start: 432,
      length: 1,
      convRule: rule22
  }, {
      start: 433,
      length: 2,
      convRule: rule43
  }, {
      start: 435,
      length: 1,
      convRule: rule21
  }, {
      start: 436,
      length: 1,
      convRule: rule22
  }, {
      start: 437,
      length: 1,
      convRule: rule21
  }, {
      start: 438,
      length: 1,
      convRule: rule22
  }, {
      start: 439,
      length: 1,
      convRule: rule44
  }, {
      start: 440,
      length: 1,
      convRule: rule21
  }, {
      start: 441,
      length: 1,
      convRule: rule22
  }, {
      start: 444,
      length: 1,
      convRule: rule21
  }, {
      start: 445,
      length: 1,
      convRule: rule22
  }, {
      start: 447,
      length: 1,
      convRule: rule46
  }, {
      start: 452,
      length: 1,
      convRule: rule47
  }, {
      start: 453,
      length: 1,
      convRule: rule48
  }, {
      start: 454,
      length: 1,
      convRule: rule49
  }, {
      start: 455,
      length: 1,
      convRule: rule47
  }, {
      start: 456,
      length: 1,
      convRule: rule48
  }, {
      start: 457,
      length: 1,
      convRule: rule49
  }, {
      start: 458,
      length: 1,
      convRule: rule47
  }, {
      start: 459,
      length: 1,
      convRule: rule48
  }, {
      start: 460,
      length: 1,
      convRule: rule49
  }, {
      start: 461,
      length: 1,
      convRule: rule21
  }, {
      start: 462,
      length: 1,
      convRule: rule22
  }, {
      start: 463,
      length: 1,
      convRule: rule21
  }, {
      start: 464,
      length: 1,
      convRule: rule22
  }, {
      start: 465,
      length: 1,
      convRule: rule21
  }, {
      start: 466,
      length: 1,
      convRule: rule22
  }, {
      start: 467,
      length: 1,
      convRule: rule21
  }, {
      start: 468,
      length: 1,
      convRule: rule22
  }, {
      start: 469,
      length: 1,
      convRule: rule21
  }, {
      start: 470,
      length: 1,
      convRule: rule22
  }, {
      start: 471,
      length: 1,
      convRule: rule21
  }, {
      start: 472,
      length: 1,
      convRule: rule22
  }, {
      start: 473,
      length: 1,
      convRule: rule21
  }, {
      start: 474,
      length: 1,
      convRule: rule22
  }, {
      start: 475,
      length: 1,
      convRule: rule21
  }, {
      start: 476,
      length: 1,
      convRule: rule22
  }, {
      start: 477,
      length: 1,
      convRule: rule50
  }, {
      start: 478,
      length: 1,
      convRule: rule21
  }, {
      start: 479,
      length: 1,
      convRule: rule22
  }, {
      start: 480,
      length: 1,
      convRule: rule21
  }, {
      start: 481,
      length: 1,
      convRule: rule22
  }, {
      start: 482,
      length: 1,
      convRule: rule21
  }, {
      start: 483,
      length: 1,
      convRule: rule22
  }, {
      start: 484,
      length: 1,
      convRule: rule21
  }, {
      start: 485,
      length: 1,
      convRule: rule22
  }, {
      start: 486,
      length: 1,
      convRule: rule21
  }, {
      start: 487,
      length: 1,
      convRule: rule22
  }, {
      start: 488,
      length: 1,
      convRule: rule21
  }, {
      start: 489,
      length: 1,
      convRule: rule22
  }, {
      start: 490,
      length: 1,
      convRule: rule21
  }, {
      start: 491,
      length: 1,
      convRule: rule22
  }, {
      start: 492,
      length: 1,
      convRule: rule21
  }, {
      start: 493,
      length: 1,
      convRule: rule22
  }, {
      start: 494,
      length: 1,
      convRule: rule21
  }, {
      start: 495,
      length: 1,
      convRule: rule22
  }, {
      start: 497,
      length: 1,
      convRule: rule47
  }, {
      start: 498,
      length: 1,
      convRule: rule48
  }, {
      start: 499,
      length: 1,
      convRule: rule49
  }, {
      start: 500,
      length: 1,
      convRule: rule21
  }, {
      start: 501,
      length: 1,
      convRule: rule22
  }, {
      start: 502,
      length: 1,
      convRule: rule51
  }, {
      start: 503,
      length: 1,
      convRule: rule52
  }, {
      start: 504,
      length: 1,
      convRule: rule21
  }, {
      start: 505,
      length: 1,
      convRule: rule22
  }, {
      start: 506,
      length: 1,
      convRule: rule21
  }, {
      start: 507,
      length: 1,
      convRule: rule22
  }, {
      start: 508,
      length: 1,
      convRule: rule21
  }, {
      start: 509,
      length: 1,
      convRule: rule22
  }, {
      start: 510,
      length: 1,
      convRule: rule21
  }, {
      start: 511,
      length: 1,
      convRule: rule22
  }, {
      start: 512,
      length: 1,
      convRule: rule21
  }, {
      start: 513,
      length: 1,
      convRule: rule22
  }, {
      start: 514,
      length: 1,
      convRule: rule21
  }, {
      start: 515,
      length: 1,
      convRule: rule22
  }, {
      start: 516,
      length: 1,
      convRule: rule21
  }, {
      start: 517,
      length: 1,
      convRule: rule22
  }, {
      start: 518,
      length: 1,
      convRule: rule21
  }, {
      start: 519,
      length: 1,
      convRule: rule22
  }, {
      start: 520,
      length: 1,
      convRule: rule21
  }, {
      start: 521,
      length: 1,
      convRule: rule22
  }, {
      start: 522,
      length: 1,
      convRule: rule21
  }, {
      start: 523,
      length: 1,
      convRule: rule22
  }, {
      start: 524,
      length: 1,
      convRule: rule21
  }, {
      start: 525,
      length: 1,
      convRule: rule22
  }, {
      start: 526,
      length: 1,
      convRule: rule21
  }, {
      start: 527,
      length: 1,
      convRule: rule22
  }, {
      start: 528,
      length: 1,
      convRule: rule21
  }, {
      start: 529,
      length: 1,
      convRule: rule22
  }, {
      start: 530,
      length: 1,
      convRule: rule21
  }, {
      start: 531,
      length: 1,
      convRule: rule22
  }, {
      start: 532,
      length: 1,
      convRule: rule21
  }, {
      start: 533,
      length: 1,
      convRule: rule22
  }, {
      start: 534,
      length: 1,
      convRule: rule21
  }, {
      start: 535,
      length: 1,
      convRule: rule22
  }, {
      start: 536,
      length: 1,
      convRule: rule21
  }, {
      start: 537,
      length: 1,
      convRule: rule22
  }, {
      start: 538,
      length: 1,
      convRule: rule21
  }, {
      start: 539,
      length: 1,
      convRule: rule22
  }, {
      start: 540,
      length: 1,
      convRule: rule21
  }, {
      start: 541,
      length: 1,
      convRule: rule22
  }, {
      start: 542,
      length: 1,
      convRule: rule21
  }, {
      start: 543,
      length: 1,
      convRule: rule22
  }, {
      start: 544,
      length: 1,
      convRule: rule53
  }, {
      start: 546,
      length: 1,
      convRule: rule21
  }, {
      start: 547,
      length: 1,
      convRule: rule22
  }, {
      start: 548,
      length: 1,
      convRule: rule21
  }, {
      start: 549,
      length: 1,
      convRule: rule22
  }, {
      start: 550,
      length: 1,
      convRule: rule21
  }, {
      start: 551,
      length: 1,
      convRule: rule22
  }, {
      start: 552,
      length: 1,
      convRule: rule21
  }, {
      start: 553,
      length: 1,
      convRule: rule22
  }, {
      start: 554,
      length: 1,
      convRule: rule21
  }, {
      start: 555,
      length: 1,
      convRule: rule22
  }, {
      start: 556,
      length: 1,
      convRule: rule21
  }, {
      start: 557,
      length: 1,
      convRule: rule22
  }, {
      start: 558,
      length: 1,
      convRule: rule21
  }, {
      start: 559,
      length: 1,
      convRule: rule22
  }, {
      start: 560,
      length: 1,
      convRule: rule21
  }, {
      start: 561,
      length: 1,
      convRule: rule22
  }, {
      start: 562,
      length: 1,
      convRule: rule21
  }, {
      start: 563,
      length: 1,
      convRule: rule22
  }, {
      start: 570,
      length: 1,
      convRule: rule54
  }, {
      start: 571,
      length: 1,
      convRule: rule21
  }, {
      start: 572,
      length: 1,
      convRule: rule22
  }, {
      start: 573,
      length: 1,
      convRule: rule55
  }, {
      start: 574,
      length: 1,
      convRule: rule56
  }, {
      start: 575,
      length: 2,
      convRule: rule57
  }, {
      start: 577,
      length: 1,
      convRule: rule21
  }, {
      start: 578,
      length: 1,
      convRule: rule22
  }, {
      start: 579,
      length: 1,
      convRule: rule58
  }, {
      start: 580,
      length: 1,
      convRule: rule59
  }, {
      start: 581,
      length: 1,
      convRule: rule60
  }, {
      start: 582,
      length: 1,
      convRule: rule21
  }, {
      start: 583,
      length: 1,
      convRule: rule22
  }, {
      start: 584,
      length: 1,
      convRule: rule21
  }, {
      start: 585,
      length: 1,
      convRule: rule22
  }, {
      start: 586,
      length: 1,
      convRule: rule21
  }, {
      start: 587,
      length: 1,
      convRule: rule22
  }, {
      start: 588,
      length: 1,
      convRule: rule21
  }, {
      start: 589,
      length: 1,
      convRule: rule22
  }, {
      start: 590,
      length: 1,
      convRule: rule21
  }, {
      start: 591,
      length: 1,
      convRule: rule22
  }, {
      start: 592,
      length: 1,
      convRule: rule61
  }, {
      start: 593,
      length: 1,
      convRule: rule62
  }, {
      start: 594,
      length: 1,
      convRule: rule63
  }, {
      start: 595,
      length: 1,
      convRule: rule64
  }, {
      start: 596,
      length: 1,
      convRule: rule65
  }, {
      start: 598,
      length: 2,
      convRule: rule66
  }, {
      start: 601,
      length: 1,
      convRule: rule67
  }, {
      start: 603,
      length: 1,
      convRule: rule68
  }, {
      start: 608,
      length: 1,
      convRule: rule66
  }, {
      start: 611,
      length: 1,
      convRule: rule69
  }, {
      start: 613,
      length: 1,
      convRule: rule70
  }, {
      start: 616,
      length: 1,
      convRule: rule71
  }, {
      start: 617,
      length: 1,
      convRule: rule72
  }, {
      start: 619,
      length: 1,
      convRule: rule73
  }, {
      start: 623,
      length: 1,
      convRule: rule72
  }, {
      start: 625,
      length: 1,
      convRule: rule74
  }, {
      start: 626,
      length: 1,
      convRule: rule75
  }, {
      start: 629,
      length: 1,
      convRule: rule76
  }, {
      start: 637,
      length: 1,
      convRule: rule77
  }, {
      start: 640,
      length: 1,
      convRule: rule78
  }, {
      start: 643,
      length: 1,
      convRule: rule78
  }, {
      start: 648,
      length: 1,
      convRule: rule78
  }, {
      start: 649,
      length: 1,
      convRule: rule79
  }, {
      start: 650,
      length: 2,
      convRule: rule80
  }, {
      start: 652,
      length: 1,
      convRule: rule81
  }, {
      start: 658,
      length: 1,
      convRule: rule82
  }, {
      start: 837,
      length: 1,
      convRule: rule85
  }, {
      start: 880,
      length: 1,
      convRule: rule21
  }, {
      start: 881,
      length: 1,
      convRule: rule22
  }, {
      start: 882,
      length: 1,
      convRule: rule21
  }, {
      start: 883,
      length: 1,
      convRule: rule22
  }, {
      start: 886,
      length: 1,
      convRule: rule21
  }, {
      start: 887,
      length: 1,
      convRule: rule22
  }, {
      start: 891,
      length: 3,
      convRule: rule40
  }, {
      start: 902,
      length: 1,
      convRule: rule86
  }, {
      start: 904,
      length: 3,
      convRule: rule87
  }, {
      start: 908,
      length: 1,
      convRule: rule88
  }, {
      start: 910,
      length: 2,
      convRule: rule89
  }, {
      start: 913,
      length: 17,
      convRule: rule9
  }, {
      start: 931,
      length: 9,
      convRule: rule9
  }, {
      start: 940,
      length: 1,
      convRule: rule90
  }, {
      start: 941,
      length: 3,
      convRule: rule91
  }, {
      start: 945,
      length: 17,
      convRule: rule12
  }, {
      start: 962,
      length: 1,
      convRule: rule92
  }, {
      start: 963,
      length: 9,
      convRule: rule12
  }, {
      start: 972,
      length: 1,
      convRule: rule93
  }, {
      start: 973,
      length: 2,
      convRule: rule94
  }, {
      start: 975,
      length: 1,
      convRule: rule95
  }, {
      start: 976,
      length: 1,
      convRule: rule96
  }, {
      start: 977,
      length: 1,
      convRule: rule97
  }, {
      start: 981,
      length: 1,
      convRule: rule99
  }, {
      start: 982,
      length: 1,
      convRule: rule100
  }, {
      start: 983,
      length: 1,
      convRule: rule101
  }, {
      start: 984,
      length: 1,
      convRule: rule21
  }, {
      start: 985,
      length: 1,
      convRule: rule22
  }, {
      start: 986,
      length: 1,
      convRule: rule21
  }, {
      start: 987,
      length: 1,
      convRule: rule22
  }, {
      start: 988,
      length: 1,
      convRule: rule21
  }, {
      start: 989,
      length: 1,
      convRule: rule22
  }, {
      start: 990,
      length: 1,
      convRule: rule21
  }, {
      start: 991,
      length: 1,
      convRule: rule22
  }, {
      start: 992,
      length: 1,
      convRule: rule21
  }, {
      start: 993,
      length: 1,
      convRule: rule22
  }, {
      start: 994,
      length: 1,
      convRule: rule21
  }, {
      start: 995,
      length: 1,
      convRule: rule22
  }, {
      start: 996,
      length: 1,
      convRule: rule21
  }, {
      start: 997,
      length: 1,
      convRule: rule22
  }, {
      start: 998,
      length: 1,
      convRule: rule21
  }, {
      start: 999,
      length: 1,
      convRule: rule22
  }, {
      start: 1000,
      length: 1,
      convRule: rule21
  }, {
      start: 1001,
      length: 1,
      convRule: rule22
  }, {
      start: 1002,
      length: 1,
      convRule: rule21
  }, {
      start: 1003,
      length: 1,
      convRule: rule22
  }, {
      start: 1004,
      length: 1,
      convRule: rule21
  }, {
      start: 1005,
      length: 1,
      convRule: rule22
  }, {
      start: 1006,
      length: 1,
      convRule: rule21
  }, {
      start: 1007,
      length: 1,
      convRule: rule22
  }, {
      start: 1008,
      length: 1,
      convRule: rule102
  }, {
      start: 1009,
      length: 1,
      convRule: rule103
  }, {
      start: 1010,
      length: 1,
      convRule: rule104
  }, {
      start: 1012,
      length: 1,
      convRule: rule105
  }, {
      start: 1013,
      length: 1,
      convRule: rule106
  }, {
      start: 1015,
      length: 1,
      convRule: rule21
  }, {
      start: 1016,
      length: 1,
      convRule: rule22
  }, {
      start: 1017,
      length: 1,
      convRule: rule107
  }, {
      start: 1018,
      length: 1,
      convRule: rule21
  }, {
      start: 1019,
      length: 1,
      convRule: rule22
  }, {
      start: 1021,
      length: 3,
      convRule: rule53
  }, {
      start: 1024,
      length: 16,
      convRule: rule108
  }, {
      start: 1040,
      length: 32,
      convRule: rule9
  }, {
      start: 1072,
      length: 32,
      convRule: rule12
  }, {
      start: 1104,
      length: 16,
      convRule: rule103
  }, {
      start: 1120,
      length: 1,
      convRule: rule21
  }, {
      start: 1121,
      length: 1,
      convRule: rule22
  }, {
      start: 1122,
      length: 1,
      convRule: rule21
  }, {
      start: 1123,
      length: 1,
      convRule: rule22
  }, {
      start: 1124,
      length: 1,
      convRule: rule21
  }, {
      start: 1125,
      length: 1,
      convRule: rule22
  }, {
      start: 1126,
      length: 1,
      convRule: rule21
  }, {
      start: 1127,
      length: 1,
      convRule: rule22
  }, {
      start: 1128,
      length: 1,
      convRule: rule21
  }, {
      start: 1129,
      length: 1,
      convRule: rule22
  }, {
      start: 1130,
      length: 1,
      convRule: rule21
  }, {
      start: 1131,
      length: 1,
      convRule: rule22
  }, {
      start: 1132,
      length: 1,
      convRule: rule21
  }, {
      start: 1133,
      length: 1,
      convRule: rule22
  }, {
      start: 1134,
      length: 1,
      convRule: rule21
  }, {
      start: 1135,
      length: 1,
      convRule: rule22
  }, {
      start: 1136,
      length: 1,
      convRule: rule21
  }, {
      start: 1137,
      length: 1,
      convRule: rule22
  }, {
      start: 1138,
      length: 1,
      convRule: rule21
  }, {
      start: 1139,
      length: 1,
      convRule: rule22
  }, {
      start: 1140,
      length: 1,
      convRule: rule21
  }, {
      start: 1141,
      length: 1,
      convRule: rule22
  }, {
      start: 1142,
      length: 1,
      convRule: rule21
  }, {
      start: 1143,
      length: 1,
      convRule: rule22
  }, {
      start: 1144,
      length: 1,
      convRule: rule21
  }, {
      start: 1145,
      length: 1,
      convRule: rule22
  }, {
      start: 1146,
      length: 1,
      convRule: rule21
  }, {
      start: 1147,
      length: 1,
      convRule: rule22
  }, {
      start: 1148,
      length: 1,
      convRule: rule21
  }, {
      start: 1149,
      length: 1,
      convRule: rule22
  }, {
      start: 1150,
      length: 1,
      convRule: rule21
  }, {
      start: 1151,
      length: 1,
      convRule: rule22
  }, {
      start: 1152,
      length: 1,
      convRule: rule21
  }, {
      start: 1153,
      length: 1,
      convRule: rule22
  }, {
      start: 1162,
      length: 1,
      convRule: rule21
  }, {
      start: 1163,
      length: 1,
      convRule: rule22
  }, {
      start: 1164,
      length: 1,
      convRule: rule21
  }, {
      start: 1165,
      length: 1,
      convRule: rule22
  }, {
      start: 1166,
      length: 1,
      convRule: rule21
  }, {
      start: 1167,
      length: 1,
      convRule: rule22
  }, {
      start: 1168,
      length: 1,
      convRule: rule21
  }, {
      start: 1169,
      length: 1,
      convRule: rule22
  }, {
      start: 1170,
      length: 1,
      convRule: rule21
  }, {
      start: 1171,
      length: 1,
      convRule: rule22
  }, {
      start: 1172,
      length: 1,
      convRule: rule21
  }, {
      start: 1173,
      length: 1,
      convRule: rule22
  }, {
      start: 1174,
      length: 1,
      convRule: rule21
  }, {
      start: 1175,
      length: 1,
      convRule: rule22
  }, {
      start: 1176,
      length: 1,
      convRule: rule21
  }, {
      start: 1177,
      length: 1,
      convRule: rule22
  }, {
      start: 1178,
      length: 1,
      convRule: rule21
  }, {
      start: 1179,
      length: 1,
      convRule: rule22
  }, {
      start: 1180,
      length: 1,
      convRule: rule21
  }, {
      start: 1181,
      length: 1,
      convRule: rule22
  }, {
      start: 1182,
      length: 1,
      convRule: rule21
  }, {
      start: 1183,
      length: 1,
      convRule: rule22
  }, {
      start: 1184,
      length: 1,
      convRule: rule21
  }, {
      start: 1185,
      length: 1,
      convRule: rule22
  }, {
      start: 1186,
      length: 1,
      convRule: rule21
  }, {
      start: 1187,
      length: 1,
      convRule: rule22
  }, {
      start: 1188,
      length: 1,
      convRule: rule21
  }, {
      start: 1189,
      length: 1,
      convRule: rule22
  }, {
      start: 1190,
      length: 1,
      convRule: rule21
  }, {
      start: 1191,
      length: 1,
      convRule: rule22
  }, {
      start: 1192,
      length: 1,
      convRule: rule21
  }, {
      start: 1193,
      length: 1,
      convRule: rule22
  }, {
      start: 1194,
      length: 1,
      convRule: rule21
  }, {
      start: 1195,
      length: 1,
      convRule: rule22
  }, {
      start: 1196,
      length: 1,
      convRule: rule21
  }, {
      start: 1197,
      length: 1,
      convRule: rule22
  }, {
      start: 1198,
      length: 1,
      convRule: rule21
  }, {
      start: 1199,
      length: 1,
      convRule: rule22
  }, {
      start: 1200,
      length: 1,
      convRule: rule21
  }, {
      start: 1201,
      length: 1,
      convRule: rule22
  }, {
      start: 1202,
      length: 1,
      convRule: rule21
  }, {
      start: 1203,
      length: 1,
      convRule: rule22
  }, {
      start: 1204,
      length: 1,
      convRule: rule21
  }, {
      start: 1205,
      length: 1,
      convRule: rule22
  }, {
      start: 1206,
      length: 1,
      convRule: rule21
  }, {
      start: 1207,
      length: 1,
      convRule: rule22
  }, {
      start: 1208,
      length: 1,
      convRule: rule21
  }, {
      start: 1209,
      length: 1,
      convRule: rule22
  }, {
      start: 1210,
      length: 1,
      convRule: rule21
  }, {
      start: 1211,
      length: 1,
      convRule: rule22
  }, {
      start: 1212,
      length: 1,
      convRule: rule21
  }, {
      start: 1213,
      length: 1,
      convRule: rule22
  }, {
      start: 1214,
      length: 1,
      convRule: rule21
  }, {
      start: 1215,
      length: 1,
      convRule: rule22
  }, {
      start: 1216,
      length: 1,
      convRule: rule110
  }, {
      start: 1217,
      length: 1,
      convRule: rule21
  }, {
      start: 1218,
      length: 1,
      convRule: rule22
  }, {
      start: 1219,
      length: 1,
      convRule: rule21
  }, {
      start: 1220,
      length: 1,
      convRule: rule22
  }, {
      start: 1221,
      length: 1,
      convRule: rule21
  }, {
      start: 1222,
      length: 1,
      convRule: rule22
  }, {
      start: 1223,
      length: 1,
      convRule: rule21
  }, {
      start: 1224,
      length: 1,
      convRule: rule22
  }, {
      start: 1225,
      length: 1,
      convRule: rule21
  }, {
      start: 1226,
      length: 1,
      convRule: rule22
  }, {
      start: 1227,
      length: 1,
      convRule: rule21
  }, {
      start: 1228,
      length: 1,
      convRule: rule22
  }, {
      start: 1229,
      length: 1,
      convRule: rule21
  }, {
      start: 1230,
      length: 1,
      convRule: rule22
  }, {
      start: 1231,
      length: 1,
      convRule: rule111
  }, {
      start: 1232,
      length: 1,
      convRule: rule21
  }, {
      start: 1233,
      length: 1,
      convRule: rule22
  }, {
      start: 1234,
      length: 1,
      convRule: rule21
  }, {
      start: 1235,
      length: 1,
      convRule: rule22
  }, {
      start: 1236,
      length: 1,
      convRule: rule21
  }, {
      start: 1237,
      length: 1,
      convRule: rule22
  }, {
      start: 1238,
      length: 1,
      convRule: rule21
  }, {
      start: 1239,
      length: 1,
      convRule: rule22
  }, {
      start: 1240,
      length: 1,
      convRule: rule21
  }, {
      start: 1241,
      length: 1,
      convRule: rule22
  }, {
      start: 1242,
      length: 1,
      convRule: rule21
  }, {
      start: 1243,
      length: 1,
      convRule: rule22
  }, {
      start: 1244,
      length: 1,
      convRule: rule21
  }, {
      start: 1245,
      length: 1,
      convRule: rule22
  }, {
      start: 1246,
      length: 1,
      convRule: rule21
  }, {
      start: 1247,
      length: 1,
      convRule: rule22
  }, {
      start: 1248,
      length: 1,
      convRule: rule21
  }, {
      start: 1249,
      length: 1,
      convRule: rule22
  }, {
      start: 1250,
      length: 1,
      convRule: rule21
  }, {
      start: 1251,
      length: 1,
      convRule: rule22
  }, {
      start: 1252,
      length: 1,
      convRule: rule21
  }, {
      start: 1253,
      length: 1,
      convRule: rule22
  }, {
      start: 1254,
      length: 1,
      convRule: rule21
  }, {
      start: 1255,
      length: 1,
      convRule: rule22
  }, {
      start: 1256,
      length: 1,
      convRule: rule21
  }, {
      start: 1257,
      length: 1,
      convRule: rule22
  }, {
      start: 1258,
      length: 1,
      convRule: rule21
  }, {
      start: 1259,
      length: 1,
      convRule: rule22
  }, {
      start: 1260,
      length: 1,
      convRule: rule21
  }, {
      start: 1261,
      length: 1,
      convRule: rule22
  }, {
      start: 1262,
      length: 1,
      convRule: rule21
  }, {
      start: 1263,
      length: 1,
      convRule: rule22
  }, {
      start: 1264,
      length: 1,
      convRule: rule21
  }, {
      start: 1265,
      length: 1,
      convRule: rule22
  }, {
      start: 1266,
      length: 1,
      convRule: rule21
  }, {
      start: 1267,
      length: 1,
      convRule: rule22
  }, {
      start: 1268,
      length: 1,
      convRule: rule21
  }, {
      start: 1269,
      length: 1,
      convRule: rule22
  }, {
      start: 1270,
      length: 1,
      convRule: rule21
  }, {
      start: 1271,
      length: 1,
      convRule: rule22
  }, {
      start: 1272,
      length: 1,
      convRule: rule21
  }, {
      start: 1273,
      length: 1,
      convRule: rule22
  }, {
      start: 1274,
      length: 1,
      convRule: rule21
  }, {
      start: 1275,
      length: 1,
      convRule: rule22
  }, {
      start: 1276,
      length: 1,
      convRule: rule21
  }, {
      start: 1277,
      length: 1,
      convRule: rule22
  }, {
      start: 1278,
      length: 1,
      convRule: rule21
  }, {
      start: 1279,
      length: 1,
      convRule: rule22
  }, {
      start: 1280,
      length: 1,
      convRule: rule21
  }, {
      start: 1281,
      length: 1,
      convRule: rule22
  }, {
      start: 1282,
      length: 1,
      convRule: rule21
  }, {
      start: 1283,
      length: 1,
      convRule: rule22
  }, {
      start: 1284,
      length: 1,
      convRule: rule21
  }, {
      start: 1285,
      length: 1,
      convRule: rule22
  }, {
      start: 1286,
      length: 1,
      convRule: rule21
  }, {
      start: 1287,
      length: 1,
      convRule: rule22
  }, {
      start: 1288,
      length: 1,
      convRule: rule21
  }, {
      start: 1289,
      length: 1,
      convRule: rule22
  }, {
      start: 1290,
      length: 1,
      convRule: rule21
  }, {
      start: 1291,
      length: 1,
      convRule: rule22
  }, {
      start: 1292,
      length: 1,
      convRule: rule21
  }, {
      start: 1293,
      length: 1,
      convRule: rule22
  }, {
      start: 1294,
      length: 1,
      convRule: rule21
  }, {
      start: 1295,
      length: 1,
      convRule: rule22
  }, {
      start: 1296,
      length: 1,
      convRule: rule21
  }, {
      start: 1297,
      length: 1,
      convRule: rule22
  }, {
      start: 1298,
      length: 1,
      convRule: rule21
  }, {
      start: 1299,
      length: 1,
      convRule: rule22
  }, {
      start: 1300,
      length: 1,
      convRule: rule21
  }, {
      start: 1301,
      length: 1,
      convRule: rule22
  }, {
      start: 1302,
      length: 1,
      convRule: rule21
  }, {
      start: 1303,
      length: 1,
      convRule: rule22
  }, {
      start: 1304,
      length: 1,
      convRule: rule21
  }, {
      start: 1305,
      length: 1,
      convRule: rule22
  }, {
      start: 1306,
      length: 1,
      convRule: rule21
  }, {
      start: 1307,
      length: 1,
      convRule: rule22
  }, {
      start: 1308,
      length: 1,
      convRule: rule21
  }, {
      start: 1309,
      length: 1,
      convRule: rule22
  }, {
      start: 1310,
      length: 1,
      convRule: rule21
  }, {
      start: 1311,
      length: 1,
      convRule: rule22
  }, {
      start: 1312,
      length: 1,
      convRule: rule21
  }, {
      start: 1313,
      length: 1,
      convRule: rule22
  }, {
      start: 1314,
      length: 1,
      convRule: rule21
  }, {
      start: 1315,
      length: 1,
      convRule: rule22
  }, {
      start: 1316,
      length: 1,
      convRule: rule21
  }, {
      start: 1317,
      length: 1,
      convRule: rule22
  }, {
      start: 1318,
      length: 1,
      convRule: rule21
  }, {
      start: 1319,
      length: 1,
      convRule: rule22
  }, {
      start: 1329,
      length: 38,
      convRule: rule112
  }, {
      start: 1377,
      length: 38,
      convRule: rule113
  }, {
      start: 4256,
      length: 38,
      convRule: rule115
  }, {
      start: 7545,
      length: 1,
      convRule: rule117
  }, {
      start: 7549,
      length: 1,
      convRule: rule118
  }, {
      start: 7680,
      length: 1,
      convRule: rule21
  }, {
      start: 7681,
      length: 1,
      convRule: rule22
  }, {
      start: 7682,
      length: 1,
      convRule: rule21
  }, {
      start: 7683,
      length: 1,
      convRule: rule22
  }, {
      start: 7684,
      length: 1,
      convRule: rule21
  }, {
      start: 7685,
      length: 1,
      convRule: rule22
  }, {
      start: 7686,
      length: 1,
      convRule: rule21
  }, {
      start: 7687,
      length: 1,
      convRule: rule22
  }, {
      start: 7688,
      length: 1,
      convRule: rule21
  }, {
      start: 7689,
      length: 1,
      convRule: rule22
  }, {
      start: 7690,
      length: 1,
      convRule: rule21
  }, {
      start: 7691,
      length: 1,
      convRule: rule22
  }, {
      start: 7692,
      length: 1,
      convRule: rule21
  }, {
      start: 7693,
      length: 1,
      convRule: rule22
  }, {
      start: 7694,
      length: 1,
      convRule: rule21
  }, {
      start: 7695,
      length: 1,
      convRule: rule22
  }, {
      start: 7696,
      length: 1,
      convRule: rule21
  }, {
      start: 7697,
      length: 1,
      convRule: rule22
  }, {
      start: 7698,
      length: 1,
      convRule: rule21
  }, {
      start: 7699,
      length: 1,
      convRule: rule22
  }, {
      start: 7700,
      length: 1,
      convRule: rule21
  }, {
      start: 7701,
      length: 1,
      convRule: rule22
  }, {
      start: 7702,
      length: 1,
      convRule: rule21
  }, {
      start: 7703,
      length: 1,
      convRule: rule22
  }, {
      start: 7704,
      length: 1,
      convRule: rule21
  }, {
      start: 7705,
      length: 1,
      convRule: rule22
  }, {
      start: 7706,
      length: 1,
      convRule: rule21
  }, {
      start: 7707,
      length: 1,
      convRule: rule22
  }, {
      start: 7708,
      length: 1,
      convRule: rule21
  }, {
      start: 7709,
      length: 1,
      convRule: rule22
  }, {
      start: 7710,
      length: 1,
      convRule: rule21
  }, {
      start: 7711,
      length: 1,
      convRule: rule22
  }, {
      start: 7712,
      length: 1,
      convRule: rule21
  }, {
      start: 7713,
      length: 1,
      convRule: rule22
  }, {
      start: 7714,
      length: 1,
      convRule: rule21
  }, {
      start: 7715,
      length: 1,
      convRule: rule22
  }, {
      start: 7716,
      length: 1,
      convRule: rule21
  }, {
      start: 7717,
      length: 1,
      convRule: rule22
  }, {
      start: 7718,
      length: 1,
      convRule: rule21
  }, {
      start: 7719,
      length: 1,
      convRule: rule22
  }, {
      start: 7720,
      length: 1,
      convRule: rule21
  }, {
      start: 7721,
      length: 1,
      convRule: rule22
  }, {
      start: 7722,
      length: 1,
      convRule: rule21
  }, {
      start: 7723,
      length: 1,
      convRule: rule22
  }, {
      start: 7724,
      length: 1,
      convRule: rule21
  }, {
      start: 7725,
      length: 1,
      convRule: rule22
  }, {
      start: 7726,
      length: 1,
      convRule: rule21
  }, {
      start: 7727,
      length: 1,
      convRule: rule22
  }, {
      start: 7728,
      length: 1,
      convRule: rule21
  }, {
      start: 7729,
      length: 1,
      convRule: rule22
  }, {
      start: 7730,
      length: 1,
      convRule: rule21
  }, {
      start: 7731,
      length: 1,
      convRule: rule22
  }, {
      start: 7732,
      length: 1,
      convRule: rule21
  }, {
      start: 7733,
      length: 1,
      convRule: rule22
  }, {
      start: 7734,
      length: 1,
      convRule: rule21
  }, {
      start: 7735,
      length: 1,
      convRule: rule22
  }, {
      start: 7736,
      length: 1,
      convRule: rule21
  }, {
      start: 7737,
      length: 1,
      convRule: rule22
  }, {
      start: 7738,
      length: 1,
      convRule: rule21
  }, {
      start: 7739,
      length: 1,
      convRule: rule22
  }, {
      start: 7740,
      length: 1,
      convRule: rule21
  }, {
      start: 7741,
      length: 1,
      convRule: rule22
  }, {
      start: 7742,
      length: 1,
      convRule: rule21
  }, {
      start: 7743,
      length: 1,
      convRule: rule22
  }, {
      start: 7744,
      length: 1,
      convRule: rule21
  }, {
      start: 7745,
      length: 1,
      convRule: rule22
  }, {
      start: 7746,
      length: 1,
      convRule: rule21
  }, {
      start: 7747,
      length: 1,
      convRule: rule22
  }, {
      start: 7748,
      length: 1,
      convRule: rule21
  }, {
      start: 7749,
      length: 1,
      convRule: rule22
  }, {
      start: 7750,
      length: 1,
      convRule: rule21
  }, {
      start: 7751,
      length: 1,
      convRule: rule22
  }, {
      start: 7752,
      length: 1,
      convRule: rule21
  }, {
      start: 7753,
      length: 1,
      convRule: rule22
  }, {
      start: 7754,
      length: 1,
      convRule: rule21
  }, {
      start: 7755,
      length: 1,
      convRule: rule22
  }, {
      start: 7756,
      length: 1,
      convRule: rule21
  }, {
      start: 7757,
      length: 1,
      convRule: rule22
  }, {
      start: 7758,
      length: 1,
      convRule: rule21
  }, {
      start: 7759,
      length: 1,
      convRule: rule22
  }, {
      start: 7760,
      length: 1,
      convRule: rule21
  }, {
      start: 7761,
      length: 1,
      convRule: rule22
  }, {
      start: 7762,
      length: 1,
      convRule: rule21
  }, {
      start: 7763,
      length: 1,
      convRule: rule22
  }, {
      start: 7764,
      length: 1,
      convRule: rule21
  }, {
      start: 7765,
      length: 1,
      convRule: rule22
  }, {
      start: 7766,
      length: 1,
      convRule: rule21
  }, {
      start: 7767,
      length: 1,
      convRule: rule22
  }, {
      start: 7768,
      length: 1,
      convRule: rule21
  }, {
      start: 7769,
      length: 1,
      convRule: rule22
  }, {
      start: 7770,
      length: 1,
      convRule: rule21
  }, {
      start: 7771,
      length: 1,
      convRule: rule22
  }, {
      start: 7772,
      length: 1,
      convRule: rule21
  }, {
      start: 7773,
      length: 1,
      convRule: rule22
  }, {
      start: 7774,
      length: 1,
      convRule: rule21
  }, {
      start: 7775,
      length: 1,
      convRule: rule22
  }, {
      start: 7776,
      length: 1,
      convRule: rule21
  }, {
      start: 7777,
      length: 1,
      convRule: rule22
  }, {
      start: 7778,
      length: 1,
      convRule: rule21
  }, {
      start: 7779,
      length: 1,
      convRule: rule22
  }, {
      start: 7780,
      length: 1,
      convRule: rule21
  }, {
      start: 7781,
      length: 1,
      convRule: rule22
  }, {
      start: 7782,
      length: 1,
      convRule: rule21
  }, {
      start: 7783,
      length: 1,
      convRule: rule22
  }, {
      start: 7784,
      length: 1,
      convRule: rule21
  }, {
      start: 7785,
      length: 1,
      convRule: rule22
  }, {
      start: 7786,
      length: 1,
      convRule: rule21
  }, {
      start: 7787,
      length: 1,
      convRule: rule22
  }, {
      start: 7788,
      length: 1,
      convRule: rule21
  }, {
      start: 7789,
      length: 1,
      convRule: rule22
  }, {
      start: 7790,
      length: 1,
      convRule: rule21
  }, {
      start: 7791,
      length: 1,
      convRule: rule22
  }, {
      start: 7792,
      length: 1,
      convRule: rule21
  }, {
      start: 7793,
      length: 1,
      convRule: rule22
  }, {
      start: 7794,
      length: 1,
      convRule: rule21
  }, {
      start: 7795,
      length: 1,
      convRule: rule22
  }, {
      start: 7796,
      length: 1,
      convRule: rule21
  }, {
      start: 7797,
      length: 1,
      convRule: rule22
  }, {
      start: 7798,
      length: 1,
      convRule: rule21
  }, {
      start: 7799,
      length: 1,
      convRule: rule22
  }, {
      start: 7800,
      length: 1,
      convRule: rule21
  }, {
      start: 7801,
      length: 1,
      convRule: rule22
  }, {
      start: 7802,
      length: 1,
      convRule: rule21
  }, {
      start: 7803,
      length: 1,
      convRule: rule22
  }, {
      start: 7804,
      length: 1,
      convRule: rule21
  }, {
      start: 7805,
      length: 1,
      convRule: rule22
  }, {
      start: 7806,
      length: 1,
      convRule: rule21
  }, {
      start: 7807,
      length: 1,
      convRule: rule22
  }, {
      start: 7808,
      length: 1,
      convRule: rule21
  }, {
      start: 7809,
      length: 1,
      convRule: rule22
  }, {
      start: 7810,
      length: 1,
      convRule: rule21
  }, {
      start: 7811,
      length: 1,
      convRule: rule22
  }, {
      start: 7812,
      length: 1,
      convRule: rule21
  }, {
      start: 7813,
      length: 1,
      convRule: rule22
  }, {
      start: 7814,
      length: 1,
      convRule: rule21
  }, {
      start: 7815,
      length: 1,
      convRule: rule22
  }, {
      start: 7816,
      length: 1,
      convRule: rule21
  }, {
      start: 7817,
      length: 1,
      convRule: rule22
  }, {
      start: 7818,
      length: 1,
      convRule: rule21
  }, {
      start: 7819,
      length: 1,
      convRule: rule22
  }, {
      start: 7820,
      length: 1,
      convRule: rule21
  }, {
      start: 7821,
      length: 1,
      convRule: rule22
  }, {
      start: 7822,
      length: 1,
      convRule: rule21
  }, {
      start: 7823,
      length: 1,
      convRule: rule22
  }, {
      start: 7824,
      length: 1,
      convRule: rule21
  }, {
      start: 7825,
      length: 1,
      convRule: rule22
  }, {
      start: 7826,
      length: 1,
      convRule: rule21
  }, {
      start: 7827,
      length: 1,
      convRule: rule22
  }, {
      start: 7828,
      length: 1,
      convRule: rule21
  }, {
      start: 7829,
      length: 1,
      convRule: rule22
  }, {
      start: 7835,
      length: 1,
      convRule: rule119
  }, {
      start: 7838,
      length: 1,
      convRule: rule120
  }, {
      start: 7840,
      length: 1,
      convRule: rule21
  }, {
      start: 7841,
      length: 1,
      convRule: rule22
  }, {
      start: 7842,
      length: 1,
      convRule: rule21
  }, {
      start: 7843,
      length: 1,
      convRule: rule22
  }, {
      start: 7844,
      length: 1,
      convRule: rule21
  }, {
      start: 7845,
      length: 1,
      convRule: rule22
  }, {
      start: 7846,
      length: 1,
      convRule: rule21
  }, {
      start: 7847,
      length: 1,
      convRule: rule22
  }, {
      start: 7848,
      length: 1,
      convRule: rule21
  }, {
      start: 7849,
      length: 1,
      convRule: rule22
  }, {
      start: 7850,
      length: 1,
      convRule: rule21
  }, {
      start: 7851,
      length: 1,
      convRule: rule22
  }, {
      start: 7852,
      length: 1,
      convRule: rule21
  }, {
      start: 7853,
      length: 1,
      convRule: rule22
  }, {
      start: 7854,
      length: 1,
      convRule: rule21
  }, {
      start: 7855,
      length: 1,
      convRule: rule22
  }, {
      start: 7856,
      length: 1,
      convRule: rule21
  }, {
      start: 7857,
      length: 1,
      convRule: rule22
  }, {
      start: 7858,
      length: 1,
      convRule: rule21
  }, {
      start: 7859,
      length: 1,
      convRule: rule22
  }, {
      start: 7860,
      length: 1,
      convRule: rule21
  }, {
      start: 7861,
      length: 1,
      convRule: rule22
  }, {
      start: 7862,
      length: 1,
      convRule: rule21
  }, {
      start: 7863,
      length: 1,
      convRule: rule22
  }, {
      start: 7864,
      length: 1,
      convRule: rule21
  }, {
      start: 7865,
      length: 1,
      convRule: rule22
  }, {
      start: 7866,
      length: 1,
      convRule: rule21
  }, {
      start: 7867,
      length: 1,
      convRule: rule22
  }, {
      start: 7868,
      length: 1,
      convRule: rule21
  }, {
      start: 7869,
      length: 1,
      convRule: rule22
  }, {
      start: 7870,
      length: 1,
      convRule: rule21
  }, {
      start: 7871,
      length: 1,
      convRule: rule22
  }, {
      start: 7872,
      length: 1,
      convRule: rule21
  }, {
      start: 7873,
      length: 1,
      convRule: rule22
  }, {
      start: 7874,
      length: 1,
      convRule: rule21
  }, {
      start: 7875,
      length: 1,
      convRule: rule22
  }, {
      start: 7876,
      length: 1,
      convRule: rule21
  }, {
      start: 7877,
      length: 1,
      convRule: rule22
  }, {
      start: 7878,
      length: 1,
      convRule: rule21
  }, {
      start: 7879,
      length: 1,
      convRule: rule22
  }, {
      start: 7880,
      length: 1,
      convRule: rule21
  }, {
      start: 7881,
      length: 1,
      convRule: rule22
  }, {
      start: 7882,
      length: 1,
      convRule: rule21
  }, {
      start: 7883,
      length: 1,
      convRule: rule22
  }, {
      start: 7884,
      length: 1,
      convRule: rule21
  }, {
      start: 7885,
      length: 1,
      convRule: rule22
  }, {
      start: 7886,
      length: 1,
      convRule: rule21
  }, {
      start: 7887,
      length: 1,
      convRule: rule22
  }, {
      start: 7888,
      length: 1,
      convRule: rule21
  }, {
      start: 7889,
      length: 1,
      convRule: rule22
  }, {
      start: 7890,
      length: 1,
      convRule: rule21
  }, {
      start: 7891,
      length: 1,
      convRule: rule22
  }, {
      start: 7892,
      length: 1,
      convRule: rule21
  }, {
      start: 7893,
      length: 1,
      convRule: rule22
  }, {
      start: 7894,
      length: 1,
      convRule: rule21
  }, {
      start: 7895,
      length: 1,
      convRule: rule22
  }, {
      start: 7896,
      length: 1,
      convRule: rule21
  }, {
      start: 7897,
      length: 1,
      convRule: rule22
  }, {
      start: 7898,
      length: 1,
      convRule: rule21
  }, {
      start: 7899,
      length: 1,
      convRule: rule22
  }, {
      start: 7900,
      length: 1,
      convRule: rule21
  }, {
      start: 7901,
      length: 1,
      convRule: rule22
  }, {
      start: 7902,
      length: 1,
      convRule: rule21
  }, {
      start: 7903,
      length: 1,
      convRule: rule22
  }, {
      start: 7904,
      length: 1,
      convRule: rule21
  }, {
      start: 7905,
      length: 1,
      convRule: rule22
  }, {
      start: 7906,
      length: 1,
      convRule: rule21
  }, {
      start: 7907,
      length: 1,
      convRule: rule22
  }, {
      start: 7908,
      length: 1,
      convRule: rule21
  }, {
      start: 7909,
      length: 1,
      convRule: rule22
  }, {
      start: 7910,
      length: 1,
      convRule: rule21
  }, {
      start: 7911,
      length: 1,
      convRule: rule22
  }, {
      start: 7912,
      length: 1,
      convRule: rule21
  }, {
      start: 7913,
      length: 1,
      convRule: rule22
  }, {
      start: 7914,
      length: 1,
      convRule: rule21
  }, {
      start: 7915,
      length: 1,
      convRule: rule22
  }, {
      start: 7916,
      length: 1,
      convRule: rule21
  }, {
      start: 7917,
      length: 1,
      convRule: rule22
  }, {
      start: 7918,
      length: 1,
      convRule: rule21
  }, {
      start: 7919,
      length: 1,
      convRule: rule22
  }, {
      start: 7920,
      length: 1,
      convRule: rule21
  }, {
      start: 7921,
      length: 1,
      convRule: rule22
  }, {
      start: 7922,
      length: 1,
      convRule: rule21
  }, {
      start: 7923,
      length: 1,
      convRule: rule22
  }, {
      start: 7924,
      length: 1,
      convRule: rule21
  }, {
      start: 7925,
      length: 1,
      convRule: rule22
  }, {
      start: 7926,
      length: 1,
      convRule: rule21
  }, {
      start: 7927,
      length: 1,
      convRule: rule22
  }, {
      start: 7928,
      length: 1,
      convRule: rule21
  }, {
      start: 7929,
      length: 1,
      convRule: rule22
  }, {
      start: 7930,
      length: 1,
      convRule: rule21
  }, {
      start: 7931,
      length: 1,
      convRule: rule22
  }, {
      start: 7932,
      length: 1,
      convRule: rule21
  }, {
      start: 7933,
      length: 1,
      convRule: rule22
  }, {
      start: 7934,
      length: 1,
      convRule: rule21
  }, {
      start: 7935,
      length: 1,
      convRule: rule22
  }, {
      start: 7936,
      length: 8,
      convRule: rule121
  }, {
      start: 7944,
      length: 8,
      convRule: rule122
  }, {
      start: 7952,
      length: 6,
      convRule: rule121
  }, {
      start: 7960,
      length: 6,
      convRule: rule122
  }, {
      start: 7968,
      length: 8,
      convRule: rule121
  }, {
      start: 7976,
      length: 8,
      convRule: rule122
  }, {
      start: 7984,
      length: 8,
      convRule: rule121
  }, {
      start: 7992,
      length: 8,
      convRule: rule122
  }, {
      start: 8000,
      length: 6,
      convRule: rule121
  }, {
      start: 8008,
      length: 6,
      convRule: rule122
  }, {
      start: 8017,
      length: 1,
      convRule: rule121
  }, {
      start: 8019,
      length: 1,
      convRule: rule121
  }, {
      start: 8021,
      length: 1,
      convRule: rule121
  }, {
      start: 8023,
      length: 1,
      convRule: rule121
  }, {
      start: 8025,
      length: 1,
      convRule: rule122
  }, {
      start: 8027,
      length: 1,
      convRule: rule122
  }, {
      start: 8029,
      length: 1,
      convRule: rule122
  }, {
      start: 8031,
      length: 1,
      convRule: rule122
  }, {
      start: 8032,
      length: 8,
      convRule: rule121
  }, {
      start: 8040,
      length: 8,
      convRule: rule122
  }, {
      start: 8048,
      length: 2,
      convRule: rule123
  }, {
      start: 8050,
      length: 4,
      convRule: rule124
  }, {
      start: 8054,
      length: 2,
      convRule: rule125
  }, {
      start: 8056,
      length: 2,
      convRule: rule126
  }, {
      start: 8058,
      length: 2,
      convRule: rule127
  }, {
      start: 8060,
      length: 2,
      convRule: rule128
  }, {
      start: 8064,
      length: 8,
      convRule: rule121
  }, {
      start: 8072,
      length: 8,
      convRule: rule129
  }, {
      start: 8080,
      length: 8,
      convRule: rule121
  }, {
      start: 8088,
      length: 8,
      convRule: rule129
  }, {
      start: 8096,
      length: 8,
      convRule: rule121
  }, {
      start: 8104,
      length: 8,
      convRule: rule129
  }, {
      start: 8112,
      length: 2,
      convRule: rule121
  }, {
      start: 8115,
      length: 1,
      convRule: rule130
  }, {
      start: 8120,
      length: 2,
      convRule: rule122
  }, {
      start: 8122,
      length: 2,
      convRule: rule131
  }, {
      start: 8124,
      length: 1,
      convRule: rule132
  }, {
      start: 8126,
      length: 1,
      convRule: rule133
  }, {
      start: 8131,
      length: 1,
      convRule: rule130
  }, {
      start: 8136,
      length: 4,
      convRule: rule134
  }, {
      start: 8140,
      length: 1,
      convRule: rule132
  }, {
      start: 8144,
      length: 2,
      convRule: rule121
  }, {
      start: 8152,
      length: 2,
      convRule: rule122
  }, {
      start: 8154,
      length: 2,
      convRule: rule135
  }, {
      start: 8160,
      length: 2,
      convRule: rule121
  }, {
      start: 8165,
      length: 1,
      convRule: rule104
  }, {
      start: 8168,
      length: 2,
      convRule: rule122
  }, {
      start: 8170,
      length: 2,
      convRule: rule136
  }, {
      start: 8172,
      length: 1,
      convRule: rule107
  }, {
      start: 8179,
      length: 1,
      convRule: rule130
  }, {
      start: 8184,
      length: 2,
      convRule: rule137
  }, {
      start: 8186,
      length: 2,
      convRule: rule138
  }, {
      start: 8188,
      length: 1,
      convRule: rule132
  }, {
      start: 8486,
      length: 1,
      convRule: rule141
  }, {
      start: 8490,
      length: 1,
      convRule: rule142
  }, {
      start: 8491,
      length: 1,
      convRule: rule143
  }, {
      start: 8498,
      length: 1,
      convRule: rule144
  }, {
      start: 8526,
      length: 1,
      convRule: rule145
  }, {
      start: 8544,
      length: 16,
      convRule: rule146
  }, {
      start: 8560,
      length: 16,
      convRule: rule147
  }, {
      start: 8579,
      length: 1,
      convRule: rule21
  }, {
      start: 8580,
      length: 1,
      convRule: rule22
  }, {
      start: 9398,
      length: 26,
      convRule: rule148
  }, {
      start: 9424,
      length: 26,
      convRule: rule149
  }, {
      start: 11264,
      length: 47,
      convRule: rule112
  }, {
      start: 11312,
      length: 47,
      convRule: rule113
  }, {
      start: 11360,
      length: 1,
      convRule: rule21
  }, {
      start: 11361,
      length: 1,
      convRule: rule22
  }, {
      start: 11362,
      length: 1,
      convRule: rule150
  }, {
      start: 11363,
      length: 1,
      convRule: rule151
  }, {
      start: 11364,
      length: 1,
      convRule: rule152
  }, {
      start: 11365,
      length: 1,
      convRule: rule153
  }, {
      start: 11366,
      length: 1,
      convRule: rule154
  }, {
      start: 11367,
      length: 1,
      convRule: rule21
  }, {
      start: 11368,
      length: 1,
      convRule: rule22
  }, {
      start: 11369,
      length: 1,
      convRule: rule21
  }, {
      start: 11370,
      length: 1,
      convRule: rule22
  }, {
      start: 11371,
      length: 1,
      convRule: rule21
  }, {
      start: 11372,
      length: 1,
      convRule: rule22
  }, {
      start: 11373,
      length: 1,
      convRule: rule155
  }, {
      start: 11374,
      length: 1,
      convRule: rule156
  }, {
      start: 11375,
      length: 1,
      convRule: rule157
  }, {
      start: 11376,
      length: 1,
      convRule: rule158
  }, {
      start: 11378,
      length: 1,
      convRule: rule21
  }, {
      start: 11379,
      length: 1,
      convRule: rule22
  }, {
      start: 11381,
      length: 1,
      convRule: rule21
  }, {
      start: 11382,
      length: 1,
      convRule: rule22
  }, {
      start: 11390,
      length: 2,
      convRule: rule159
  }, {
      start: 11392,
      length: 1,
      convRule: rule21
  }, {
      start: 11393,
      length: 1,
      convRule: rule22
  }, {
      start: 11394,
      length: 1,
      convRule: rule21
  }, {
      start: 11395,
      length: 1,
      convRule: rule22
  }, {
      start: 11396,
      length: 1,
      convRule: rule21
  }, {
      start: 11397,
      length: 1,
      convRule: rule22
  }, {
      start: 11398,
      length: 1,
      convRule: rule21
  }, {
      start: 11399,
      length: 1,
      convRule: rule22
  }, {
      start: 11400,
      length: 1,
      convRule: rule21
  }, {
      start: 11401,
      length: 1,
      convRule: rule22
  }, {
      start: 11402,
      length: 1,
      convRule: rule21
  }, {
      start: 11403,
      length: 1,
      convRule: rule22
  }, {
      start: 11404,
      length: 1,
      convRule: rule21
  }, {
      start: 11405,
      length: 1,
      convRule: rule22
  }, {
      start: 11406,
      length: 1,
      convRule: rule21
  }, {
      start: 11407,
      length: 1,
      convRule: rule22
  }, {
      start: 11408,
      length: 1,
      convRule: rule21
  }, {
      start: 11409,
      length: 1,
      convRule: rule22
  }, {
      start: 11410,
      length: 1,
      convRule: rule21
  }, {
      start: 11411,
      length: 1,
      convRule: rule22
  }, {
      start: 11412,
      length: 1,
      convRule: rule21
  }, {
      start: 11413,
      length: 1,
      convRule: rule22
  }, {
      start: 11414,
      length: 1,
      convRule: rule21
  }, {
      start: 11415,
      length: 1,
      convRule: rule22
  }, {
      start: 11416,
      length: 1,
      convRule: rule21
  }, {
      start: 11417,
      length: 1,
      convRule: rule22
  }, {
      start: 11418,
      length: 1,
      convRule: rule21
  }, {
      start: 11419,
      length: 1,
      convRule: rule22
  }, {
      start: 11420,
      length: 1,
      convRule: rule21
  }, {
      start: 11421,
      length: 1,
      convRule: rule22
  }, {
      start: 11422,
      length: 1,
      convRule: rule21
  }, {
      start: 11423,
      length: 1,
      convRule: rule22
  }, {
      start: 11424,
      length: 1,
      convRule: rule21
  }, {
      start: 11425,
      length: 1,
      convRule: rule22
  }, {
      start: 11426,
      length: 1,
      convRule: rule21
  }, {
      start: 11427,
      length: 1,
      convRule: rule22
  }, {
      start: 11428,
      length: 1,
      convRule: rule21
  }, {
      start: 11429,
      length: 1,
      convRule: rule22
  }, {
      start: 11430,
      length: 1,
      convRule: rule21
  }, {
      start: 11431,
      length: 1,
      convRule: rule22
  }, {
      start: 11432,
      length: 1,
      convRule: rule21
  }, {
      start: 11433,
      length: 1,
      convRule: rule22
  }, {
      start: 11434,
      length: 1,
      convRule: rule21
  }, {
      start: 11435,
      length: 1,
      convRule: rule22
  }, {
      start: 11436,
      length: 1,
      convRule: rule21
  }, {
      start: 11437,
      length: 1,
      convRule: rule22
  }, {
      start: 11438,
      length: 1,
      convRule: rule21
  }, {
      start: 11439,
      length: 1,
      convRule: rule22
  }, {
      start: 11440,
      length: 1,
      convRule: rule21
  }, {
      start: 11441,
      length: 1,
      convRule: rule22
  }, {
      start: 11442,
      length: 1,
      convRule: rule21
  }, {
      start: 11443,
      length: 1,
      convRule: rule22
  }, {
      start: 11444,
      length: 1,
      convRule: rule21
  }, {
      start: 11445,
      length: 1,
      convRule: rule22
  }, {
      start: 11446,
      length: 1,
      convRule: rule21
  }, {
      start: 11447,
      length: 1,
      convRule: rule22
  }, {
      start: 11448,
      length: 1,
      convRule: rule21
  }, {
      start: 11449,
      length: 1,
      convRule: rule22
  }, {
      start: 11450,
      length: 1,
      convRule: rule21
  }, {
      start: 11451,
      length: 1,
      convRule: rule22
  }, {
      start: 11452,
      length: 1,
      convRule: rule21
  }, {
      start: 11453,
      length: 1,
      convRule: rule22
  }, {
      start: 11454,
      length: 1,
      convRule: rule21
  }, {
      start: 11455,
      length: 1,
      convRule: rule22
  }, {
      start: 11456,
      length: 1,
      convRule: rule21
  }, {
      start: 11457,
      length: 1,
      convRule: rule22
  }, {
      start: 11458,
      length: 1,
      convRule: rule21
  }, {
      start: 11459,
      length: 1,
      convRule: rule22
  }, {
      start: 11460,
      length: 1,
      convRule: rule21
  }, {
      start: 11461,
      length: 1,
      convRule: rule22
  }, {
      start: 11462,
      length: 1,
      convRule: rule21
  }, {
      start: 11463,
      length: 1,
      convRule: rule22
  }, {
      start: 11464,
      length: 1,
      convRule: rule21
  }, {
      start: 11465,
      length: 1,
      convRule: rule22
  }, {
      start: 11466,
      length: 1,
      convRule: rule21
  }, {
      start: 11467,
      length: 1,
      convRule: rule22
  }, {
      start: 11468,
      length: 1,
      convRule: rule21
  }, {
      start: 11469,
      length: 1,
      convRule: rule22
  }, {
      start: 11470,
      length: 1,
      convRule: rule21
  }, {
      start: 11471,
      length: 1,
      convRule: rule22
  }, {
      start: 11472,
      length: 1,
      convRule: rule21
  }, {
      start: 11473,
      length: 1,
      convRule: rule22
  }, {
      start: 11474,
      length: 1,
      convRule: rule21
  }, {
      start: 11475,
      length: 1,
      convRule: rule22
  }, {
      start: 11476,
      length: 1,
      convRule: rule21
  }, {
      start: 11477,
      length: 1,
      convRule: rule22
  }, {
      start: 11478,
      length: 1,
      convRule: rule21
  }, {
      start: 11479,
      length: 1,
      convRule: rule22
  }, {
      start: 11480,
      length: 1,
      convRule: rule21
  }, {
      start: 11481,
      length: 1,
      convRule: rule22
  }, {
      start: 11482,
      length: 1,
      convRule: rule21
  }, {
      start: 11483,
      length: 1,
      convRule: rule22
  }, {
      start: 11484,
      length: 1,
      convRule: rule21
  }, {
      start: 11485,
      length: 1,
      convRule: rule22
  }, {
      start: 11486,
      length: 1,
      convRule: rule21
  }, {
      start: 11487,
      length: 1,
      convRule: rule22
  }, {
      start: 11488,
      length: 1,
      convRule: rule21
  }, {
      start: 11489,
      length: 1,
      convRule: rule22
  }, {
      start: 11490,
      length: 1,
      convRule: rule21
  }, {
      start: 11491,
      length: 1,
      convRule: rule22
  }, {
      start: 11499,
      length: 1,
      convRule: rule21
  }, {
      start: 11500,
      length: 1,
      convRule: rule22
  }, {
      start: 11501,
      length: 1,
      convRule: rule21
  }, {
      start: 11502,
      length: 1,
      convRule: rule22
  }, {
      start: 11520,
      length: 38,
      convRule: rule160
  }, {
      start: 42560,
      length: 1,
      convRule: rule21
  }, {
      start: 42561,
      length: 1,
      convRule: rule22
  }, {
      start: 42562,
      length: 1,
      convRule: rule21
  }, {
      start: 42563,
      length: 1,
      convRule: rule22
  }, {
      start: 42564,
      length: 1,
      convRule: rule21
  }, {
      start: 42565,
      length: 1,
      convRule: rule22
  }, {
      start: 42566,
      length: 1,
      convRule: rule21
  }, {
      start: 42567,
      length: 1,
      convRule: rule22
  }, {
      start: 42568,
      length: 1,
      convRule: rule21
  }, {
      start: 42569,
      length: 1,
      convRule: rule22
  }, {
      start: 42570,
      length: 1,
      convRule: rule21
  }, {
      start: 42571,
      length: 1,
      convRule: rule22
  }, {
      start: 42572,
      length: 1,
      convRule: rule21
  }, {
      start: 42573,
      length: 1,
      convRule: rule22
  }, {
      start: 42574,
      length: 1,
      convRule: rule21
  }, {
      start: 42575,
      length: 1,
      convRule: rule22
  }, {
      start: 42576,
      length: 1,
      convRule: rule21
  }, {
      start: 42577,
      length: 1,
      convRule: rule22
  }, {
      start: 42578,
      length: 1,
      convRule: rule21
  }, {
      start: 42579,
      length: 1,
      convRule: rule22
  }, {
      start: 42580,
      length: 1,
      convRule: rule21
  }, {
      start: 42581,
      length: 1,
      convRule: rule22
  }, {
      start: 42582,
      length: 1,
      convRule: rule21
  }, {
      start: 42583,
      length: 1,
      convRule: rule22
  }, {
      start: 42584,
      length: 1,
      convRule: rule21
  }, {
      start: 42585,
      length: 1,
      convRule: rule22
  }, {
      start: 42586,
      length: 1,
      convRule: rule21
  }, {
      start: 42587,
      length: 1,
      convRule: rule22
  }, {
      start: 42588,
      length: 1,
      convRule: rule21
  }, {
      start: 42589,
      length: 1,
      convRule: rule22
  }, {
      start: 42590,
      length: 1,
      convRule: rule21
  }, {
      start: 42591,
      length: 1,
      convRule: rule22
  }, {
      start: 42592,
      length: 1,
      convRule: rule21
  }, {
      start: 42593,
      length: 1,
      convRule: rule22
  }, {
      start: 42594,
      length: 1,
      convRule: rule21
  }, {
      start: 42595,
      length: 1,
      convRule: rule22
  }, {
      start: 42596,
      length: 1,
      convRule: rule21
  }, {
      start: 42597,
      length: 1,
      convRule: rule22
  }, {
      start: 42598,
      length: 1,
      convRule: rule21
  }, {
      start: 42599,
      length: 1,
      convRule: rule22
  }, {
      start: 42600,
      length: 1,
      convRule: rule21
  }, {
      start: 42601,
      length: 1,
      convRule: rule22
  }, {
      start: 42602,
      length: 1,
      convRule: rule21
  }, {
      start: 42603,
      length: 1,
      convRule: rule22
  }, {
      start: 42604,
      length: 1,
      convRule: rule21
  }, {
      start: 42605,
      length: 1,
      convRule: rule22
  }, {
      start: 42624,
      length: 1,
      convRule: rule21
  }, {
      start: 42625,
      length: 1,
      convRule: rule22
  }, {
      start: 42626,
      length: 1,
      convRule: rule21
  }, {
      start: 42627,
      length: 1,
      convRule: rule22
  }, {
      start: 42628,
      length: 1,
      convRule: rule21
  }, {
      start: 42629,
      length: 1,
      convRule: rule22
  }, {
      start: 42630,
      length: 1,
      convRule: rule21
  }, {
      start: 42631,
      length: 1,
      convRule: rule22
  }, {
      start: 42632,
      length: 1,
      convRule: rule21
  }, {
      start: 42633,
      length: 1,
      convRule: rule22
  }, {
      start: 42634,
      length: 1,
      convRule: rule21
  }, {
      start: 42635,
      length: 1,
      convRule: rule22
  }, {
      start: 42636,
      length: 1,
      convRule: rule21
  }, {
      start: 42637,
      length: 1,
      convRule: rule22
  }, {
      start: 42638,
      length: 1,
      convRule: rule21
  }, {
      start: 42639,
      length: 1,
      convRule: rule22
  }, {
      start: 42640,
      length: 1,
      convRule: rule21
  }, {
      start: 42641,
      length: 1,
      convRule: rule22
  }, {
      start: 42642,
      length: 1,
      convRule: rule21
  }, {
      start: 42643,
      length: 1,
      convRule: rule22
  }, {
      start: 42644,
      length: 1,
      convRule: rule21
  }, {
      start: 42645,
      length: 1,
      convRule: rule22
  }, {
      start: 42646,
      length: 1,
      convRule: rule21
  }, {
      start: 42647,
      length: 1,
      convRule: rule22
  }, {
      start: 42786,
      length: 1,
      convRule: rule21
  }, {
      start: 42787,
      length: 1,
      convRule: rule22
  }, {
      start: 42788,
      length: 1,
      convRule: rule21
  }, {
      start: 42789,
      length: 1,
      convRule: rule22
  }, {
      start: 42790,
      length: 1,
      convRule: rule21
  }, {
      start: 42791,
      length: 1,
      convRule: rule22
  }, {
      start: 42792,
      length: 1,
      convRule: rule21
  }, {
      start: 42793,
      length: 1,
      convRule: rule22
  }, {
      start: 42794,
      length: 1,
      convRule: rule21
  }, {
      start: 42795,
      length: 1,
      convRule: rule22
  }, {
      start: 42796,
      length: 1,
      convRule: rule21
  }, {
      start: 42797,
      length: 1,
      convRule: rule22
  }, {
      start: 42798,
      length: 1,
      convRule: rule21
  }, {
      start: 42799,
      length: 1,
      convRule: rule22
  }, {
      start: 42802,
      length: 1,
      convRule: rule21
  }, {
      start: 42803,
      length: 1,
      convRule: rule22
  }, {
      start: 42804,
      length: 1,
      convRule: rule21
  }, {
      start: 42805,
      length: 1,
      convRule: rule22
  }, {
      start: 42806,
      length: 1,
      convRule: rule21
  }, {
      start: 42807,
      length: 1,
      convRule: rule22
  }, {
      start: 42808,
      length: 1,
      convRule: rule21
  }, {
      start: 42809,
      length: 1,
      convRule: rule22
  }, {
      start: 42810,
      length: 1,
      convRule: rule21
  }, {
      start: 42811,
      length: 1,
      convRule: rule22
  }, {
      start: 42812,
      length: 1,
      convRule: rule21
  }, {
      start: 42813,
      length: 1,
      convRule: rule22
  }, {
      start: 42814,
      length: 1,
      convRule: rule21
  }, {
      start: 42815,
      length: 1,
      convRule: rule22
  }, {
      start: 42816,
      length: 1,
      convRule: rule21
  }, {
      start: 42817,
      length: 1,
      convRule: rule22
  }, {
      start: 42818,
      length: 1,
      convRule: rule21
  }, {
      start: 42819,
      length: 1,
      convRule: rule22
  }, {
      start: 42820,
      length: 1,
      convRule: rule21
  }, {
      start: 42821,
      length: 1,
      convRule: rule22
  }, {
      start: 42822,
      length: 1,
      convRule: rule21
  }, {
      start: 42823,
      length: 1,
      convRule: rule22
  }, {
      start: 42824,
      length: 1,
      convRule: rule21
  }, {
      start: 42825,
      length: 1,
      convRule: rule22
  }, {
      start: 42826,
      length: 1,
      convRule: rule21
  }, {
      start: 42827,
      length: 1,
      convRule: rule22
  }, {
      start: 42828,
      length: 1,
      convRule: rule21
  }, {
      start: 42829,
      length: 1,
      convRule: rule22
  }, {
      start: 42830,
      length: 1,
      convRule: rule21
  }, {
      start: 42831,
      length: 1,
      convRule: rule22
  }, {
      start: 42832,
      length: 1,
      convRule: rule21
  }, {
      start: 42833,
      length: 1,
      convRule: rule22
  }, {
      start: 42834,
      length: 1,
      convRule: rule21
  }, {
      start: 42835,
      length: 1,
      convRule: rule22
  }, {
      start: 42836,
      length: 1,
      convRule: rule21
  }, {
      start: 42837,
      length: 1,
      convRule: rule22
  }, {
      start: 42838,
      length: 1,
      convRule: rule21
  }, {
      start: 42839,
      length: 1,
      convRule: rule22
  }, {
      start: 42840,
      length: 1,
      convRule: rule21
  }, {
      start: 42841,
      length: 1,
      convRule: rule22
  }, {
      start: 42842,
      length: 1,
      convRule: rule21
  }, {
      start: 42843,
      length: 1,
      convRule: rule22
  }, {
      start: 42844,
      length: 1,
      convRule: rule21
  }, {
      start: 42845,
      length: 1,
      convRule: rule22
  }, {
      start: 42846,
      length: 1,
      convRule: rule21
  }, {
      start: 42847,
      length: 1,
      convRule: rule22
  }, {
      start: 42848,
      length: 1,
      convRule: rule21
  }, {
      start: 42849,
      length: 1,
      convRule: rule22
  }, {
      start: 42850,
      length: 1,
      convRule: rule21
  }, {
      start: 42851,
      length: 1,
      convRule: rule22
  }, {
      start: 42852,
      length: 1,
      convRule: rule21
  }, {
      start: 42853,
      length: 1,
      convRule: rule22
  }, {
      start: 42854,
      length: 1,
      convRule: rule21
  }, {
      start: 42855,
      length: 1,
      convRule: rule22
  }, {
      start: 42856,
      length: 1,
      convRule: rule21
  }, {
      start: 42857,
      length: 1,
      convRule: rule22
  }, {
      start: 42858,
      length: 1,
      convRule: rule21
  }, {
      start: 42859,
      length: 1,
      convRule: rule22
  }, {
      start: 42860,
      length: 1,
      convRule: rule21
  }, {
      start: 42861,
      length: 1,
      convRule: rule22
  }, {
      start: 42862,
      length: 1,
      convRule: rule21
  }, {
      start: 42863,
      length: 1,
      convRule: rule22
  }, {
      start: 42873,
      length: 1,
      convRule: rule21
  }, {
      start: 42874,
      length: 1,
      convRule: rule22
  }, {
      start: 42875,
      length: 1,
      convRule: rule21
  }, {
      start: 42876,
      length: 1,
      convRule: rule22
  }, {
      start: 42877,
      length: 1,
      convRule: rule161
  }, {
      start: 42878,
      length: 1,
      convRule: rule21
  }, {
      start: 42879,
      length: 1,
      convRule: rule22
  }, {
      start: 42880,
      length: 1,
      convRule: rule21
  }, {
      start: 42881,
      length: 1,
      convRule: rule22
  }, {
      start: 42882,
      length: 1,
      convRule: rule21
  }, {
      start: 42883,
      length: 1,
      convRule: rule22
  }, {
      start: 42884,
      length: 1,
      convRule: rule21
  }, {
      start: 42885,
      length: 1,
      convRule: rule22
  }, {
      start: 42886,
      length: 1,
      convRule: rule21
  }, {
      start: 42887,
      length: 1,
      convRule: rule22
  }, {
      start: 42891,
      length: 1,
      convRule: rule21
  }, {
      start: 42892,
      length: 1,
      convRule: rule22
  }, {
      start: 42893,
      length: 1,
      convRule: rule162
  }, {
      start: 42896,
      length: 1,
      convRule: rule21
  }, {
      start: 42897,
      length: 1,
      convRule: rule22
  }, {
      start: 42912,
      length: 1,
      convRule: rule21
  }, {
      start: 42913,
      length: 1,
      convRule: rule22
  }, {
      start: 42914,
      length: 1,
      convRule: rule21
  }, {
      start: 42915,
      length: 1,
      convRule: rule22
  }, {
      start: 42916,
      length: 1,
      convRule: rule21
  }, {
      start: 42917,
      length: 1,
      convRule: rule22
  }, {
      start: 42918,
      length: 1,
      convRule: rule21
  }, {
      start: 42919,
      length: 1,
      convRule: rule22
  }, {
      start: 42920,
      length: 1,
      convRule: rule21
  }, {
      start: 42921,
      length: 1,
      convRule: rule22
  }, {
      start: 65313,
      length: 26,
      convRule: rule9
  }, {
      start: 65345,
      length: 26,
      convRule: rule12
  }, {
      start: 66560,
      length: 40,
      convRule: rule165
  }, {
      start: 66600,
      length: 40,
      convRule: rule166
  } ];
  var bsearch = function (a) {
      return function (array) {
          return function (size) {
              return function (compare) {
                  var go = function ($copy_i) {
                      return function ($copy_k) {
                          var $tco_var_i = $copy_i;
                          var $tco_done = false;
                          var $tco_result;
                          function $tco_loop(i, k) {
                              if (i > k) {
                                  $tco_done = true;
                                  return Data_Maybe.Nothing.value;
                              };
                              if (Data_Boolean.otherwise) {
                                  var j = Data_Int.floor(Data_Int.toNumber(i + k | 0) / 2.0);
                                  var v = compare(a)(array[j]);
                                  if (v instanceof Data_Ordering.EQ) {
                                      $tco_done = true;
                                      return new Data_Maybe.Just(array[j]);
                                  };
                                  if (v instanceof Data_Ordering.GT) {
                                      $tco_var_i = j + 1 | 0;
                                      $copy_k = k;
                                      return;
                                  };
                                  $tco_var_i = i;
                                  $copy_k = j - 1 | 0;
                                  return;
                              };
                              throw new Error("Failed pattern match at Data.Char.Unicode.Internal (line 4783, column 5 - line 4789, column 49): " + [ i.constructor.name, k.constructor.name ]);
                          };
                          while (!$tco_done) {
                              $tco_result = $tco_loop($tco_var_i, $copy_k);
                          };
                          return $tco_result;
                      };
                  };
                  return go(0)(size);
              };
          };
      };
  };
  var blkCmp = function (v) {
      return function (v1) {
          if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
              return Data_Ordering.EQ.value;
          };
          if (v.start > v1.start) {
              return Data_Ordering.GT.value;
          };
          if (Data_Boolean.otherwise) {
              return Data_Ordering.LT.value;
          };
          throw new Error("Failed pattern match at Data.Char.Unicode.Internal (line 4760, column 1 - line 4760, column 45): " + [ v.constructor.name, v1.constructor.name ]);
      };
  };
  var getRule = function (blocks) {
      return function (unichar) {
          return function (size) {
              var key = {
                  start: unichar,
                  length: 1,
                  convRule: nullrule
              };
              var maybeCharBlock = bsearch(key)(blocks)(size)(blkCmp);
              if (maybeCharBlock instanceof Data_Maybe.Nothing) {
                  return Data_Maybe.Nothing.value;
              };
              if (maybeCharBlock instanceof Data_Maybe.Just) {
                  return new Data_Maybe.Just(maybeCharBlock.value0.convRule);
              };
              throw new Error("Failed pattern match at Data.Char.Unicode.Internal (line 4773, column 8 - line 4775, column 62): " + [ maybeCharBlock.constructor.name ]);
          };
      };
  };
  var caseConv = function (f) {
      return function ($$char) {
          var maybeConversionRule = getRule(convchars)($$char)(numConvBlocks);
          if (maybeConversionRule instanceof Data_Maybe.Nothing) {
              return $$char;
          };
          if (maybeConversionRule instanceof Data_Maybe.Just) {
              return $$char + f(maybeConversionRule.value0) | 0;
          };
          throw new Error("Failed pattern match at Data.Char.Unicode.Internal (line 4850, column 8 - line 4852, column 55): " + [ maybeConversionRule.constructor.name ]);
      };
  };
  var uTowlower = caseConv(function (v) {
      return v.lowdist;
  });
  var uTowupper = caseConv(function (v) {
      return v.updist;
  });
  var checkAttrS = function (categories) {
      return function ($$char) {
          var maybeConversionRule = getRule(spacechars)($$char)(numSpaceBlocks);
          if (maybeConversionRule instanceof Data_Maybe.Nothing) {
              return false;
          };
          if (maybeConversionRule instanceof Data_Maybe.Just) {
              return Data_Maybe.isJust(Data_Array.elemIndex(Data_Eq.eqInt)(maybeConversionRule.value0.category)(categories));
          };
          throw new Error("Failed pattern match at Data.Char.Unicode.Internal (line 4807, column 8 - line 4809, column 92): " + [ maybeConversionRule.constructor.name ]);
      };
  };
  var uIswspace = checkAttrS([ gencatZS ]);
  var allchars = [ {
      start: 0,
      length: 32,
      convRule: rule0
  }, {
      start: 32,
      length: 1,
      convRule: rule1
  }, {
      start: 33,
      length: 3,
      convRule: rule2
  }, {
      start: 36,
      length: 1,
      convRule: rule3
  }, {
      start: 37,
      length: 3,
      convRule: rule2
  }, {
      start: 40,
      length: 1,
      convRule: rule4
  }, {
      start: 41,
      length: 1,
      convRule: rule5
  }, {
      start: 42,
      length: 1,
      convRule: rule2
  }, {
      start: 43,
      length: 1,
      convRule: rule6
  }, {
      start: 44,
      length: 1,
      convRule: rule2
  }, {
      start: 45,
      length: 1,
      convRule: rule7
  }, {
      start: 46,
      length: 2,
      convRule: rule2
  }, {
      start: 48,
      length: 10,
      convRule: rule8
  }, {
      start: 58,
      length: 2,
      convRule: rule2
  }, {
      start: 60,
      length: 3,
      convRule: rule6
  }, {
      start: 63,
      length: 2,
      convRule: rule2
  }, {
      start: 65,
      length: 26,
      convRule: rule9
  }, {
      start: 91,
      length: 1,
      convRule: rule4
  }, {
      start: 92,
      length: 1,
      convRule: rule2
  }, {
      start: 93,
      length: 1,
      convRule: rule5
  }, {
      start: 94,
      length: 1,
      convRule: rule10
  }, {
      start: 95,
      length: 1,
      convRule: rule11
  }, {
      start: 96,
      length: 1,
      convRule: rule10
  }, {
      start: 97,
      length: 26,
      convRule: rule12
  }, {
      start: 123,
      length: 1,
      convRule: rule4
  }, {
      start: 124,
      length: 1,
      convRule: rule6
  }, {
      start: 125,
      length: 1,
      convRule: rule5
  }, {
      start: 126,
      length: 1,
      convRule: rule6
  }, {
      start: 127,
      length: 33,
      convRule: rule0
  }, {
      start: 160,
      length: 1,
      convRule: rule1
  }, {
      start: 161,
      length: 1,
      convRule: rule2
  }, {
      start: 162,
      length: 4,
      convRule: rule3
  }, {
      start: 166,
      length: 2,
      convRule: rule13
  }, {
      start: 168,
      length: 1,
      convRule: rule10
  }, {
      start: 169,
      length: 1,
      convRule: rule13
  }, {
      start: 170,
      length: 1,
      convRule: rule14
  }, {
      start: 171,
      length: 1,
      convRule: rule15
  }, {
      start: 172,
      length: 1,
      convRule: rule6
  }, {
      start: 173,
      length: 1,
      convRule: rule16
  }, {
      start: 174,
      length: 1,
      convRule: rule13
  }, {
      start: 175,
      length: 1,
      convRule: rule10
  }, {
      start: 176,
      length: 1,
      convRule: rule13
  }, {
      start: 177,
      length: 1,
      convRule: rule6
  }, {
      start: 178,
      length: 2,
      convRule: rule17
  }, {
      start: 180,
      length: 1,
      convRule: rule10
  }, {
      start: 181,
      length: 1,
      convRule: rule18
  }, {
      start: 182,
      length: 1,
      convRule: rule13
  }, {
      start: 183,
      length: 1,
      convRule: rule2
  }, {
      start: 184,
      length: 1,
      convRule: rule10
  }, {
      start: 185,
      length: 1,
      convRule: rule17
  }, {
      start: 186,
      length: 1,
      convRule: rule14
  }, {
      start: 187,
      length: 1,
      convRule: rule19
  }, {
      start: 188,
      length: 3,
      convRule: rule17
  }, {
      start: 191,
      length: 1,
      convRule: rule2
  }, {
      start: 192,
      length: 23,
      convRule: rule9
  }, {
      start: 215,
      length: 1,
      convRule: rule6
  }, {
      start: 216,
      length: 7,
      convRule: rule9
  }, {
      start: 223,
      length: 1,
      convRule: rule14
  }, {
      start: 224,
      length: 23,
      convRule: rule12
  }, {
      start: 247,
      length: 1,
      convRule: rule6
  }, {
      start: 248,
      length: 7,
      convRule: rule12
  }, {
      start: 255,
      length: 1,
      convRule: rule20
  }, {
      start: 256,
      length: 1,
      convRule: rule21
  }, {
      start: 257,
      length: 1,
      convRule: rule22
  }, {
      start: 258,
      length: 1,
      convRule: rule21
  }, {
      start: 259,
      length: 1,
      convRule: rule22
  }, {
      start: 260,
      length: 1,
      convRule: rule21
  }, {
      start: 261,
      length: 1,
      convRule: rule22
  }, {
      start: 262,
      length: 1,
      convRule: rule21
  }, {
      start: 263,
      length: 1,
      convRule: rule22
  }, {
      start: 264,
      length: 1,
      convRule: rule21
  }, {
      start: 265,
      length: 1,
      convRule: rule22
  }, {
      start: 266,
      length: 1,
      convRule: rule21
  }, {
      start: 267,
      length: 1,
      convRule: rule22
  }, {
      start: 268,
      length: 1,
      convRule: rule21
  }, {
      start: 269,
      length: 1,
      convRule: rule22
  }, {
      start: 270,
      length: 1,
      convRule: rule21
  }, {
      start: 271,
      length: 1,
      convRule: rule22
  }, {
      start: 272,
      length: 1,
      convRule: rule21
  }, {
      start: 273,
      length: 1,
      convRule: rule22
  }, {
      start: 274,
      length: 1,
      convRule: rule21
  }, {
      start: 275,
      length: 1,
      convRule: rule22
  }, {
      start: 276,
      length: 1,
      convRule: rule21
  }, {
      start: 277,
      length: 1,
      convRule: rule22
  }, {
      start: 278,
      length: 1,
      convRule: rule21
  }, {
      start: 279,
      length: 1,
      convRule: rule22
  }, {
      start: 280,
      length: 1,
      convRule: rule21
  }, {
      start: 281,
      length: 1,
      convRule: rule22
  }, {
      start: 282,
      length: 1,
      convRule: rule21
  }, {
      start: 283,
      length: 1,
      convRule: rule22
  }, {
      start: 284,
      length: 1,
      convRule: rule21
  }, {
      start: 285,
      length: 1,
      convRule: rule22
  }, {
      start: 286,
      length: 1,
      convRule: rule21
  }, {
      start: 287,
      length: 1,
      convRule: rule22
  }, {
      start: 288,
      length: 1,
      convRule: rule21
  }, {
      start: 289,
      length: 1,
      convRule: rule22
  }, {
      start: 290,
      length: 1,
      convRule: rule21
  }, {
      start: 291,
      length: 1,
      convRule: rule22
  }, {
      start: 292,
      length: 1,
      convRule: rule21
  }, {
      start: 293,
      length: 1,
      convRule: rule22
  }, {
      start: 294,
      length: 1,
      convRule: rule21
  }, {
      start: 295,
      length: 1,
      convRule: rule22
  }, {
      start: 296,
      length: 1,
      convRule: rule21
  }, {
      start: 297,
      length: 1,
      convRule: rule22
  }, {
      start: 298,
      length: 1,
      convRule: rule21
  }, {
      start: 299,
      length: 1,
      convRule: rule22
  }, {
      start: 300,
      length: 1,
      convRule: rule21
  }, {
      start: 301,
      length: 1,
      convRule: rule22
  }, {
      start: 302,
      length: 1,
      convRule: rule21
  }, {
      start: 303,
      length: 1,
      convRule: rule22
  }, {
      start: 304,
      length: 1,
      convRule: rule23
  }, {
      start: 305,
      length: 1,
      convRule: rule24
  }, {
      start: 306,
      length: 1,
      convRule: rule21
  }, {
      start: 307,
      length: 1,
      convRule: rule22
  }, {
      start: 308,
      length: 1,
      convRule: rule21
  }, {
      start: 309,
      length: 1,
      convRule: rule22
  }, {
      start: 310,
      length: 1,
      convRule: rule21
  }, {
      start: 311,
      length: 1,
      convRule: rule22
  }, {
      start: 312,
      length: 1,
      convRule: rule14
  }, {
      start: 313,
      length: 1,
      convRule: rule21
  }, {
      start: 314,
      length: 1,
      convRule: rule22
  }, {
      start: 315,
      length: 1,
      convRule: rule21
  }, {
      start: 316,
      length: 1,
      convRule: rule22
  }, {
      start: 317,
      length: 1,
      convRule: rule21
  }, {
      start: 318,
      length: 1,
      convRule: rule22
  }, {
      start: 319,
      length: 1,
      convRule: rule21
  }, {
      start: 320,
      length: 1,
      convRule: rule22
  }, {
      start: 321,
      length: 1,
      convRule: rule21
  }, {
      start: 322,
      length: 1,
      convRule: rule22
  }, {
      start: 323,
      length: 1,
      convRule: rule21
  }, {
      start: 324,
      length: 1,
      convRule: rule22
  }, {
      start: 325,
      length: 1,
      convRule: rule21
  }, {
      start: 326,
      length: 1,
      convRule: rule22
  }, {
      start: 327,
      length: 1,
      convRule: rule21
  }, {
      start: 328,
      length: 1,
      convRule: rule22
  }, {
      start: 329,
      length: 1,
      convRule: rule14
  }, {
      start: 330,
      length: 1,
      convRule: rule21
  }, {
      start: 331,
      length: 1,
      convRule: rule22
  }, {
      start: 332,
      length: 1,
      convRule: rule21
  }, {
      start: 333,
      length: 1,
      convRule: rule22
  }, {
      start: 334,
      length: 1,
      convRule: rule21
  }, {
      start: 335,
      length: 1,
      convRule: rule22
  }, {
      start: 336,
      length: 1,
      convRule: rule21
  }, {
      start: 337,
      length: 1,
      convRule: rule22
  }, {
      start: 338,
      length: 1,
      convRule: rule21
  }, {
      start: 339,
      length: 1,
      convRule: rule22
  }, {
      start: 340,
      length: 1,
      convRule: rule21
  }, {
      start: 341,
      length: 1,
      convRule: rule22
  }, {
      start: 342,
      length: 1,
      convRule: rule21
  }, {
      start: 343,
      length: 1,
      convRule: rule22
  }, {
      start: 344,
      length: 1,
      convRule: rule21
  }, {
      start: 345,
      length: 1,
      convRule: rule22
  }, {
      start: 346,
      length: 1,
      convRule: rule21
  }, {
      start: 347,
      length: 1,
      convRule: rule22
  }, {
      start: 348,
      length: 1,
      convRule: rule21
  }, {
      start: 349,
      length: 1,
      convRule: rule22
  }, {
      start: 350,
      length: 1,
      convRule: rule21
  }, {
      start: 351,
      length: 1,
      convRule: rule22
  }, {
      start: 352,
      length: 1,
      convRule: rule21
  }, {
      start: 353,
      length: 1,
      convRule: rule22
  }, {
      start: 354,
      length: 1,
      convRule: rule21
  }, {
      start: 355,
      length: 1,
      convRule: rule22
  }, {
      start: 356,
      length: 1,
      convRule: rule21
  }, {
      start: 357,
      length: 1,
      convRule: rule22
  }, {
      start: 358,
      length: 1,
      convRule: rule21
  }, {
      start: 359,
      length: 1,
      convRule: rule22
  }, {
      start: 360,
      length: 1,
      convRule: rule21
  }, {
      start: 361,
      length: 1,
      convRule: rule22
  }, {
      start: 362,
      length: 1,
      convRule: rule21
  }, {
      start: 363,
      length: 1,
      convRule: rule22
  }, {
      start: 364,
      length: 1,
      convRule: rule21
  }, {
      start: 365,
      length: 1,
      convRule: rule22
  }, {
      start: 366,
      length: 1,
      convRule: rule21
  }, {
      start: 367,
      length: 1,
      convRule: rule22
  }, {
      start: 368,
      length: 1,
      convRule: rule21
  }, {
      start: 369,
      length: 1,
      convRule: rule22
  }, {
      start: 370,
      length: 1,
      convRule: rule21
  }, {
      start: 371,
      length: 1,
      convRule: rule22
  }, {
      start: 372,
      length: 1,
      convRule: rule21
  }, {
      start: 373,
      length: 1,
      convRule: rule22
  }, {
      start: 374,
      length: 1,
      convRule: rule21
  }, {
      start: 375,
      length: 1,
      convRule: rule22
  }, {
      start: 376,
      length: 1,
      convRule: rule25
  }, {
      start: 377,
      length: 1,
      convRule: rule21
  }, {
      start: 378,
      length: 1,
      convRule: rule22
  }, {
      start: 379,
      length: 1,
      convRule: rule21
  }, {
      start: 380,
      length: 1,
      convRule: rule22
  }, {
      start: 381,
      length: 1,
      convRule: rule21
  }, {
      start: 382,
      length: 1,
      convRule: rule22
  }, {
      start: 383,
      length: 1,
      convRule: rule26
  }, {
      start: 384,
      length: 1,
      convRule: rule27
  }, {
      start: 385,
      length: 1,
      convRule: rule28
  }, {
      start: 386,
      length: 1,
      convRule: rule21
  }, {
      start: 387,
      length: 1,
      convRule: rule22
  }, {
      start: 388,
      length: 1,
      convRule: rule21
  }, {
      start: 389,
      length: 1,
      convRule: rule22
  }, {
      start: 390,
      length: 1,
      convRule: rule29
  }, {
      start: 391,
      length: 1,
      convRule: rule21
  }, {
      start: 392,
      length: 1,
      convRule: rule22
  }, {
      start: 393,
      length: 2,
      convRule: rule30
  }, {
      start: 395,
      length: 1,
      convRule: rule21
  }, {
      start: 396,
      length: 1,
      convRule: rule22
  }, {
      start: 397,
      length: 1,
      convRule: rule14
  }, {
      start: 398,
      length: 1,
      convRule: rule31
  }, {
      start: 399,
      length: 1,
      convRule: rule32
  }, {
      start: 400,
      length: 1,
      convRule: rule33
  }, {
      start: 401,
      length: 1,
      convRule: rule21
  }, {
      start: 402,
      length: 1,
      convRule: rule22
  }, {
      start: 403,
      length: 1,
      convRule: rule30
  }, {
      start: 404,
      length: 1,
      convRule: rule34
  }, {
      start: 405,
      length: 1,
      convRule: rule35
  }, {
      start: 406,
      length: 1,
      convRule: rule36
  }, {
      start: 407,
      length: 1,
      convRule: rule37
  }, {
      start: 408,
      length: 1,
      convRule: rule21
  }, {
      start: 409,
      length: 1,
      convRule: rule22
  }, {
      start: 410,
      length: 1,
      convRule: rule38
  }, {
      start: 411,
      length: 1,
      convRule: rule14
  }, {
      start: 412,
      length: 1,
      convRule: rule36
  }, {
      start: 413,
      length: 1,
      convRule: rule39
  }, {
      start: 414,
      length: 1,
      convRule: rule40
  }, {
      start: 415,
      length: 1,
      convRule: rule41
  }, {
      start: 416,
      length: 1,
      convRule: rule21
  }, {
      start: 417,
      length: 1,
      convRule: rule22
  }, {
      start: 418,
      length: 1,
      convRule: rule21
  }, {
      start: 419,
      length: 1,
      convRule: rule22
  }, {
      start: 420,
      length: 1,
      convRule: rule21
  }, {
      start: 421,
      length: 1,
      convRule: rule22
  }, {
      start: 422,
      length: 1,
      convRule: rule42
  }, {
      start: 423,
      length: 1,
      convRule: rule21
  }, {
      start: 424,
      length: 1,
      convRule: rule22
  }, {
      start: 425,
      length: 1,
      convRule: rule42
  }, {
      start: 426,
      length: 2,
      convRule: rule14
  }, {
      start: 428,
      length: 1,
      convRule: rule21
  }, {
      start: 429,
      length: 1,
      convRule: rule22
  }, {
      start: 430,
      length: 1,
      convRule: rule42
  }, {
      start: 431,
      length: 1,
      convRule: rule21
  }, {
      start: 432,
      length: 1,
      convRule: rule22
  }, {
      start: 433,
      length: 2,
      convRule: rule43
  }, {
      start: 435,
      length: 1,
      convRule: rule21
  }, {
      start: 436,
      length: 1,
      convRule: rule22
  }, {
      start: 437,
      length: 1,
      convRule: rule21
  }, {
      start: 438,
      length: 1,
      convRule: rule22
  }, {
      start: 439,
      length: 1,
      convRule: rule44
  }, {
      start: 440,
      length: 1,
      convRule: rule21
  }, {
      start: 441,
      length: 1,
      convRule: rule22
  }, {
      start: 442,
      length: 1,
      convRule: rule14
  }, {
      start: 443,
      length: 1,
      convRule: rule45
  }, {
      start: 444,
      length: 1,
      convRule: rule21
  }, {
      start: 445,
      length: 1,
      convRule: rule22
  }, {
      start: 446,
      length: 1,
      convRule: rule14
  }, {
      start: 447,
      length: 1,
      convRule: rule46
  }, {
      start: 448,
      length: 4,
      convRule: rule45
  }, {
      start: 452,
      length: 1,
      convRule: rule47
  }, {
      start: 453,
      length: 1,
      convRule: rule48
  }, {
      start: 454,
      length: 1,
      convRule: rule49
  }, {
      start: 455,
      length: 1,
      convRule: rule47
  }, {
      start: 456,
      length: 1,
      convRule: rule48
  }, {
      start: 457,
      length: 1,
      convRule: rule49
  }, {
      start: 458,
      length: 1,
      convRule: rule47
  }, {
      start: 459,
      length: 1,
      convRule: rule48
  }, {
      start: 460,
      length: 1,
      convRule: rule49
  }, {
      start: 461,
      length: 1,
      convRule: rule21
  }, {
      start: 462,
      length: 1,
      convRule: rule22
  }, {
      start: 463,
      length: 1,
      convRule: rule21
  }, {
      start: 464,
      length: 1,
      convRule: rule22
  }, {
      start: 465,
      length: 1,
      convRule: rule21
  }, {
      start: 466,
      length: 1,
      convRule: rule22
  }, {
      start: 467,
      length: 1,
      convRule: rule21
  }, {
      start: 468,
      length: 1,
      convRule: rule22
  }, {
      start: 469,
      length: 1,
      convRule: rule21
  }, {
      start: 470,
      length: 1,
      convRule: rule22
  }, {
      start: 471,
      length: 1,
      convRule: rule21
  }, {
      start: 472,
      length: 1,
      convRule: rule22
  }, {
      start: 473,
      length: 1,
      convRule: rule21
  }, {
      start: 474,
      length: 1,
      convRule: rule22
  }, {
      start: 475,
      length: 1,
      convRule: rule21
  }, {
      start: 476,
      length: 1,
      convRule: rule22
  }, {
      start: 477,
      length: 1,
      convRule: rule50
  }, {
      start: 478,
      length: 1,
      convRule: rule21
  }, {
      start: 479,
      length: 1,
      convRule: rule22
  }, {
      start: 480,
      length: 1,
      convRule: rule21
  }, {
      start: 481,
      length: 1,
      convRule: rule22
  }, {
      start: 482,
      length: 1,
      convRule: rule21
  }, {
      start: 483,
      length: 1,
      convRule: rule22
  }, {
      start: 484,
      length: 1,
      convRule: rule21
  }, {
      start: 485,
      length: 1,
      convRule: rule22
  }, {
      start: 486,
      length: 1,
      convRule: rule21
  }, {
      start: 487,
      length: 1,
      convRule: rule22
  }, {
      start: 488,
      length: 1,
      convRule: rule21
  }, {
      start: 489,
      length: 1,
      convRule: rule22
  }, {
      start: 490,
      length: 1,
      convRule: rule21
  }, {
      start: 491,
      length: 1,
      convRule: rule22
  }, {
      start: 492,
      length: 1,
      convRule: rule21
  }, {
      start: 493,
      length: 1,
      convRule: rule22
  }, {
      start: 494,
      length: 1,
      convRule: rule21
  }, {
      start: 495,
      length: 1,
      convRule: rule22
  }, {
      start: 496,
      length: 1,
      convRule: rule14
  }, {
      start: 497,
      length: 1,
      convRule: rule47
  }, {
      start: 498,
      length: 1,
      convRule: rule48
  }, {
      start: 499,
      length: 1,
      convRule: rule49
  }, {
      start: 500,
      length: 1,
      convRule: rule21
  }, {
      start: 501,
      length: 1,
      convRule: rule22
  }, {
      start: 502,
      length: 1,
      convRule: rule51
  }, {
      start: 503,
      length: 1,
      convRule: rule52
  }, {
      start: 504,
      length: 1,
      convRule: rule21
  }, {
      start: 505,
      length: 1,
      convRule: rule22
  }, {
      start: 506,
      length: 1,
      convRule: rule21
  }, {
      start: 507,
      length: 1,
      convRule: rule22
  }, {
      start: 508,
      length: 1,
      convRule: rule21
  }, {
      start: 509,
      length: 1,
      convRule: rule22
  }, {
      start: 510,
      length: 1,
      convRule: rule21
  }, {
      start: 511,
      length: 1,
      convRule: rule22
  }, {
      start: 512,
      length: 1,
      convRule: rule21
  }, {
      start: 513,
      length: 1,
      convRule: rule22
  }, {
      start: 514,
      length: 1,
      convRule: rule21
  }, {
      start: 515,
      length: 1,
      convRule: rule22
  }, {
      start: 516,
      length: 1,
      convRule: rule21
  }, {
      start: 517,
      length: 1,
      convRule: rule22
  }, {
      start: 518,
      length: 1,
      convRule: rule21
  }, {
      start: 519,
      length: 1,
      convRule: rule22
  }, {
      start: 520,
      length: 1,
      convRule: rule21
  }, {
      start: 521,
      length: 1,
      convRule: rule22
  }, {
      start: 522,
      length: 1,
      convRule: rule21
  }, {
      start: 523,
      length: 1,
      convRule: rule22
  }, {
      start: 524,
      length: 1,
      convRule: rule21
  }, {
      start: 525,
      length: 1,
      convRule: rule22
  }, {
      start: 526,
      length: 1,
      convRule: rule21
  }, {
      start: 527,
      length: 1,
      convRule: rule22
  }, {
      start: 528,
      length: 1,
      convRule: rule21
  }, {
      start: 529,
      length: 1,
      convRule: rule22
  }, {
      start: 530,
      length: 1,
      convRule: rule21
  }, {
      start: 531,
      length: 1,
      convRule: rule22
  }, {
      start: 532,
      length: 1,
      convRule: rule21
  }, {
      start: 533,
      length: 1,
      convRule: rule22
  }, {
      start: 534,
      length: 1,
      convRule: rule21
  }, {
      start: 535,
      length: 1,
      convRule: rule22
  }, {
      start: 536,
      length: 1,
      convRule: rule21
  }, {
      start: 537,
      length: 1,
      convRule: rule22
  }, {
      start: 538,
      length: 1,
      convRule: rule21
  }, {
      start: 539,
      length: 1,
      convRule: rule22
  }, {
      start: 540,
      length: 1,
      convRule: rule21
  }, {
      start: 541,
      length: 1,
      convRule: rule22
  }, {
      start: 542,
      length: 1,
      convRule: rule21
  }, {
      start: 543,
      length: 1,
      convRule: rule22
  }, {
      start: 544,
      length: 1,
      convRule: rule53
  }, {
      start: 545,
      length: 1,
      convRule: rule14
  }, {
      start: 546,
      length: 1,
      convRule: rule21
  }, {
      start: 547,
      length: 1,
      convRule: rule22
  }, {
      start: 548,
      length: 1,
      convRule: rule21
  }, {
      start: 549,
      length: 1,
      convRule: rule22
  }, {
      start: 550,
      length: 1,
      convRule: rule21
  }, {
      start: 551,
      length: 1,
      convRule: rule22
  }, {
      start: 552,
      length: 1,
      convRule: rule21
  }, {
      start: 553,
      length: 1,
      convRule: rule22
  }, {
      start: 554,
      length: 1,
      convRule: rule21
  }, {
      start: 555,
      length: 1,
      convRule: rule22
  }, {
      start: 556,
      length: 1,
      convRule: rule21
  }, {
      start: 557,
      length: 1,
      convRule: rule22
  }, {
      start: 558,
      length: 1,
      convRule: rule21
  }, {
      start: 559,
      length: 1,
      convRule: rule22
  }, {
      start: 560,
      length: 1,
      convRule: rule21
  }, {
      start: 561,
      length: 1,
      convRule: rule22
  }, {
      start: 562,
      length: 1,
      convRule: rule21
  }, {
      start: 563,
      length: 1,
      convRule: rule22
  }, {
      start: 564,
      length: 6,
      convRule: rule14
  }, {
      start: 570,
      length: 1,
      convRule: rule54
  }, {
      start: 571,
      length: 1,
      convRule: rule21
  }, {
      start: 572,
      length: 1,
      convRule: rule22
  }, {
      start: 573,
      length: 1,
      convRule: rule55
  }, {
      start: 574,
      length: 1,
      convRule: rule56
  }, {
      start: 575,
      length: 2,
      convRule: rule57
  }, {
      start: 577,
      length: 1,
      convRule: rule21
  }, {
      start: 578,
      length: 1,
      convRule: rule22
  }, {
      start: 579,
      length: 1,
      convRule: rule58
  }, {
      start: 580,
      length: 1,
      convRule: rule59
  }, {
      start: 581,
      length: 1,
      convRule: rule60
  }, {
      start: 582,
      length: 1,
      convRule: rule21
  }, {
      start: 583,
      length: 1,
      convRule: rule22
  }, {
      start: 584,
      length: 1,
      convRule: rule21
  }, {
      start: 585,
      length: 1,
      convRule: rule22
  }, {
      start: 586,
      length: 1,
      convRule: rule21
  }, {
      start: 587,
      length: 1,
      convRule: rule22
  }, {
      start: 588,
      length: 1,
      convRule: rule21
  }, {
      start: 589,
      length: 1,
      convRule: rule22
  }, {
      start: 590,
      length: 1,
      convRule: rule21
  }, {
      start: 591,
      length: 1,
      convRule: rule22
  }, {
      start: 592,
      length: 1,
      convRule: rule61
  }, {
      start: 593,
      length: 1,
      convRule: rule62
  }, {
      start: 594,
      length: 1,
      convRule: rule63
  }, {
      start: 595,
      length: 1,
      convRule: rule64
  }, {
      start: 596,
      length: 1,
      convRule: rule65
  }, {
      start: 597,
      length: 1,
      convRule: rule14
  }, {
      start: 598,
      length: 2,
      convRule: rule66
  }, {
      start: 600,
      length: 1,
      convRule: rule14
  }, {
      start: 601,
      length: 1,
      convRule: rule67
  }, {
      start: 602,
      length: 1,
      convRule: rule14
  }, {
      start: 603,
      length: 1,
      convRule: rule68
  }, {
      start: 604,
      length: 4,
      convRule: rule14
  }, {
      start: 608,
      length: 1,
      convRule: rule66
  }, {
      start: 609,
      length: 2,
      convRule: rule14
  }, {
      start: 611,
      length: 1,
      convRule: rule69
  }, {
      start: 612,
      length: 1,
      convRule: rule14
  }, {
      start: 613,
      length: 1,
      convRule: rule70
  }, {
      start: 614,
      length: 2,
      convRule: rule14
  }, {
      start: 616,
      length: 1,
      convRule: rule71
  }, {
      start: 617,
      length: 1,
      convRule: rule72
  }, {
      start: 618,
      length: 1,
      convRule: rule14
  }, {
      start: 619,
      length: 1,
      convRule: rule73
  }, {
      start: 620,
      length: 3,
      convRule: rule14
  }, {
      start: 623,
      length: 1,
      convRule: rule72
  }, {
      start: 624,
      length: 1,
      convRule: rule14
  }, {
      start: 625,
      length: 1,
      convRule: rule74
  }, {
      start: 626,
      length: 1,
      convRule: rule75
  }, {
      start: 627,
      length: 2,
      convRule: rule14
  }, {
      start: 629,
      length: 1,
      convRule: rule76
  }, {
      start: 630,
      length: 7,
      convRule: rule14
  }, {
      start: 637,
      length: 1,
      convRule: rule77
  }, {
      start: 638,
      length: 2,
      convRule: rule14
  }, {
      start: 640,
      length: 1,
      convRule: rule78
  }, {
      start: 641,
      length: 2,
      convRule: rule14
  }, {
      start: 643,
      length: 1,
      convRule: rule78
  }, {
      start: 644,
      length: 4,
      convRule: rule14
  }, {
      start: 648,
      length: 1,
      convRule: rule78
  }, {
      start: 649,
      length: 1,
      convRule: rule79
  }, {
      start: 650,
      length: 2,
      convRule: rule80
  }, {
      start: 652,
      length: 1,
      convRule: rule81
  }, {
      start: 653,
      length: 5,
      convRule: rule14
  }, {
      start: 658,
      length: 1,
      convRule: rule82
  }, {
      start: 659,
      length: 1,
      convRule: rule14
  }, {
      start: 660,
      length: 1,
      convRule: rule45
  }, {
      start: 661,
      length: 27,
      convRule: rule14
  }, {
      start: 688,
      length: 18,
      convRule: rule83
  }, {
      start: 706,
      length: 4,
      convRule: rule10
  }, {
      start: 710,
      length: 12,
      convRule: rule83
  }, {
      start: 722,
      length: 14,
      convRule: rule10
  }, {
      start: 736,
      length: 5,
      convRule: rule83
  }, {
      start: 741,
      length: 7,
      convRule: rule10
  }, {
      start: 748,
      length: 1,
      convRule: rule83
  }, {
      start: 749,
      length: 1,
      convRule: rule10
  }, {
      start: 750,
      length: 1,
      convRule: rule83
  }, {
      start: 751,
      length: 17,
      convRule: rule10
  }, {
      start: 768,
      length: 69,
      convRule: rule84
  }, {
      start: 837,
      length: 1,
      convRule: rule85
  }, {
      start: 838,
      length: 42,
      convRule: rule84
  }, {
      start: 880,
      length: 1,
      convRule: rule21
  }, {
      start: 881,
      length: 1,
      convRule: rule22
  }, {
      start: 882,
      length: 1,
      convRule: rule21
  }, {
      start: 883,
      length: 1,
      convRule: rule22
  }, {
      start: 884,
      length: 1,
      convRule: rule83
  }, {
      start: 885,
      length: 1,
      convRule: rule10
  }, {
      start: 886,
      length: 1,
      convRule: rule21
  }, {
      start: 887,
      length: 1,
      convRule: rule22
  }, {
      start: 890,
      length: 1,
      convRule: rule83
  }, {
      start: 891,
      length: 3,
      convRule: rule40
  }, {
      start: 894,
      length: 1,
      convRule: rule2
  }, {
      start: 900,
      length: 2,
      convRule: rule10
  }, {
      start: 902,
      length: 1,
      convRule: rule86
  }, {
      start: 903,
      length: 1,
      convRule: rule2
  }, {
      start: 904,
      length: 3,
      convRule: rule87
  }, {
      start: 908,
      length: 1,
      convRule: rule88
  }, {
      start: 910,
      length: 2,
      convRule: rule89
  }, {
      start: 912,
      length: 1,
      convRule: rule14
  }, {
      start: 913,
      length: 17,
      convRule: rule9
  }, {
      start: 931,
      length: 9,
      convRule: rule9
  }, {
      start: 940,
      length: 1,
      convRule: rule90
  }, {
      start: 941,
      length: 3,
      convRule: rule91
  }, {
      start: 944,
      length: 1,
      convRule: rule14
  }, {
      start: 945,
      length: 17,
      convRule: rule12
  }, {
      start: 962,
      length: 1,
      convRule: rule92
  }, {
      start: 963,
      length: 9,
      convRule: rule12
  }, {
      start: 972,
      length: 1,
      convRule: rule93
  }, {
      start: 973,
      length: 2,
      convRule: rule94
  }, {
      start: 975,
      length: 1,
      convRule: rule95
  }, {
      start: 976,
      length: 1,
      convRule: rule96
  }, {
      start: 977,
      length: 1,
      convRule: rule97
  }, {
      start: 978,
      length: 3,
      convRule: rule98
  }, {
      start: 981,
      length: 1,
      convRule: rule99
  }, {
      start: 982,
      length: 1,
      convRule: rule100
  }, {
      start: 983,
      length: 1,
      convRule: rule101
  }, {
      start: 984,
      length: 1,
      convRule: rule21
  }, {
      start: 985,
      length: 1,
      convRule: rule22
  }, {
      start: 986,
      length: 1,
      convRule: rule21
  }, {
      start: 987,
      length: 1,
      convRule: rule22
  }, {
      start: 988,
      length: 1,
      convRule: rule21
  }, {
      start: 989,
      length: 1,
      convRule: rule22
  }, {
      start: 990,
      length: 1,
      convRule: rule21
  }, {
      start: 991,
      length: 1,
      convRule: rule22
  }, {
      start: 992,
      length: 1,
      convRule: rule21
  }, {
      start: 993,
      length: 1,
      convRule: rule22
  }, {
      start: 994,
      length: 1,
      convRule: rule21
  }, {
      start: 995,
      length: 1,
      convRule: rule22
  }, {
      start: 996,
      length: 1,
      convRule: rule21
  }, {
      start: 997,
      length: 1,
      convRule: rule22
  }, {
      start: 998,
      length: 1,
      convRule: rule21
  }, {
      start: 999,
      length: 1,
      convRule: rule22
  }, {
      start: 1000,
      length: 1,
      convRule: rule21
  }, {
      start: 1001,
      length: 1,
      convRule: rule22
  }, {
      start: 1002,
      length: 1,
      convRule: rule21
  }, {
      start: 1003,
      length: 1,
      convRule: rule22
  }, {
      start: 1004,
      length: 1,
      convRule: rule21
  }, {
      start: 1005,
      length: 1,
      convRule: rule22
  }, {
      start: 1006,
      length: 1,
      convRule: rule21
  }, {
      start: 1007,
      length: 1,
      convRule: rule22
  }, {
      start: 1008,
      length: 1,
      convRule: rule102
  }, {
      start: 1009,
      length: 1,
      convRule: rule103
  }, {
      start: 1010,
      length: 1,
      convRule: rule104
  }, {
      start: 1011,
      length: 1,
      convRule: rule14
  }, {
      start: 1012,
      length: 1,
      convRule: rule105
  }, {
      start: 1013,
      length: 1,
      convRule: rule106
  }, {
      start: 1014,
      length: 1,
      convRule: rule6
  }, {
      start: 1015,
      length: 1,
      convRule: rule21
  }, {
      start: 1016,
      length: 1,
      convRule: rule22
  }, {
      start: 1017,
      length: 1,
      convRule: rule107
  }, {
      start: 1018,
      length: 1,
      convRule: rule21
  }, {
      start: 1019,
      length: 1,
      convRule: rule22
  }, {
      start: 1020,
      length: 1,
      convRule: rule14
  }, {
      start: 1021,
      length: 3,
      convRule: rule53
  }, {
      start: 1024,
      length: 16,
      convRule: rule108
  }, {
      start: 1040,
      length: 32,
      convRule: rule9
  }, {
      start: 1072,
      length: 32,
      convRule: rule12
  }, {
      start: 1104,
      length: 16,
      convRule: rule103
  }, {
      start: 1120,
      length: 1,
      convRule: rule21
  }, {
      start: 1121,
      length: 1,
      convRule: rule22
  }, {
      start: 1122,
      length: 1,
      convRule: rule21
  }, {
      start: 1123,
      length: 1,
      convRule: rule22
  }, {
      start: 1124,
      length: 1,
      convRule: rule21
  }, {
      start: 1125,
      length: 1,
      convRule: rule22
  }, {
      start: 1126,
      length: 1,
      convRule: rule21
  }, {
      start: 1127,
      length: 1,
      convRule: rule22
  }, {
      start: 1128,
      length: 1,
      convRule: rule21
  }, {
      start: 1129,
      length: 1,
      convRule: rule22
  }, {
      start: 1130,
      length: 1,
      convRule: rule21
  }, {
      start: 1131,
      length: 1,
      convRule: rule22
  }, {
      start: 1132,
      length: 1,
      convRule: rule21
  }, {
      start: 1133,
      length: 1,
      convRule: rule22
  }, {
      start: 1134,
      length: 1,
      convRule: rule21
  }, {
      start: 1135,
      length: 1,
      convRule: rule22
  }, {
      start: 1136,
      length: 1,
      convRule: rule21
  }, {
      start: 1137,
      length: 1,
      convRule: rule22
  }, {
      start: 1138,
      length: 1,
      convRule: rule21
  }, {
      start: 1139,
      length: 1,
      convRule: rule22
  }, {
      start: 1140,
      length: 1,
      convRule: rule21
  }, {
      start: 1141,
      length: 1,
      convRule: rule22
  }, {
      start: 1142,
      length: 1,
      convRule: rule21
  }, {
      start: 1143,
      length: 1,
      convRule: rule22
  }, {
      start: 1144,
      length: 1,
      convRule: rule21
  }, {
      start: 1145,
      length: 1,
      convRule: rule22
  }, {
      start: 1146,
      length: 1,
      convRule: rule21
  }, {
      start: 1147,
      length: 1,
      convRule: rule22
  }, {
      start: 1148,
      length: 1,
      convRule: rule21
  }, {
      start: 1149,
      length: 1,
      convRule: rule22
  }, {
      start: 1150,
      length: 1,
      convRule: rule21
  }, {
      start: 1151,
      length: 1,
      convRule: rule22
  }, {
      start: 1152,
      length: 1,
      convRule: rule21
  }, {
      start: 1153,
      length: 1,
      convRule: rule22
  }, {
      start: 1154,
      length: 1,
      convRule: rule13
  }, {
      start: 1155,
      length: 5,
      convRule: rule84
  }, {
      start: 1160,
      length: 2,
      convRule: rule109
  }, {
      start: 1162,
      length: 1,
      convRule: rule21
  }, {
      start: 1163,
      length: 1,
      convRule: rule22
  }, {
      start: 1164,
      length: 1,
      convRule: rule21
  }, {
      start: 1165,
      length: 1,
      convRule: rule22
  }, {
      start: 1166,
      length: 1,
      convRule: rule21
  }, {
      start: 1167,
      length: 1,
      convRule: rule22
  }, {
      start: 1168,
      length: 1,
      convRule: rule21
  }, {
      start: 1169,
      length: 1,
      convRule: rule22
  }, {
      start: 1170,
      length: 1,
      convRule: rule21
  }, {
      start: 1171,
      length: 1,
      convRule: rule22
  }, {
      start: 1172,
      length: 1,
      convRule: rule21
  }, {
      start: 1173,
      length: 1,
      convRule: rule22
  }, {
      start: 1174,
      length: 1,
      convRule: rule21
  }, {
      start: 1175,
      length: 1,
      convRule: rule22
  }, {
      start: 1176,
      length: 1,
      convRule: rule21
  }, {
      start: 1177,
      length: 1,
      convRule: rule22
  }, {
      start: 1178,
      length: 1,
      convRule: rule21
  }, {
      start: 1179,
      length: 1,
      convRule: rule22
  }, {
      start: 1180,
      length: 1,
      convRule: rule21
  }, {
      start: 1181,
      length: 1,
      convRule: rule22
  }, {
      start: 1182,
      length: 1,
      convRule: rule21
  }, {
      start: 1183,
      length: 1,
      convRule: rule22
  }, {
      start: 1184,
      length: 1,
      convRule: rule21
  }, {
      start: 1185,
      length: 1,
      convRule: rule22
  }, {
      start: 1186,
      length: 1,
      convRule: rule21
  }, {
      start: 1187,
      length: 1,
      convRule: rule22
  }, {
      start: 1188,
      length: 1,
      convRule: rule21
  }, {
      start: 1189,
      length: 1,
      convRule: rule22
  }, {
      start: 1190,
      length: 1,
      convRule: rule21
  }, {
      start: 1191,
      length: 1,
      convRule: rule22
  }, {
      start: 1192,
      length: 1,
      convRule: rule21
  }, {
      start: 1193,
      length: 1,
      convRule: rule22
  }, {
      start: 1194,
      length: 1,
      convRule: rule21
  }, {
      start: 1195,
      length: 1,
      convRule: rule22
  }, {
      start: 1196,
      length: 1,
      convRule: rule21
  }, {
      start: 1197,
      length: 1,
      convRule: rule22
  }, {
      start: 1198,
      length: 1,
      convRule: rule21
  }, {
      start: 1199,
      length: 1,
      convRule: rule22
  }, {
      start: 1200,
      length: 1,
      convRule: rule21
  }, {
      start: 1201,
      length: 1,
      convRule: rule22
  }, {
      start: 1202,
      length: 1,
      convRule: rule21
  }, {
      start: 1203,
      length: 1,
      convRule: rule22
  }, {
      start: 1204,
      length: 1,
      convRule: rule21
  }, {
      start: 1205,
      length: 1,
      convRule: rule22
  }, {
      start: 1206,
      length: 1,
      convRule: rule21
  }, {
      start: 1207,
      length: 1,
      convRule: rule22
  }, {
      start: 1208,
      length: 1,
      convRule: rule21
  }, {
      start: 1209,
      length: 1,
      convRule: rule22
  }, {
      start: 1210,
      length: 1,
      convRule: rule21
  }, {
      start: 1211,
      length: 1,
      convRule: rule22
  }, {
      start: 1212,
      length: 1,
      convRule: rule21
  }, {
      start: 1213,
      length: 1,
      convRule: rule22
  }, {
      start: 1214,
      length: 1,
      convRule: rule21
  }, {
      start: 1215,
      length: 1,
      convRule: rule22
  }, {
      start: 1216,
      length: 1,
      convRule: rule110
  }, {
      start: 1217,
      length: 1,
      convRule: rule21
  }, {
      start: 1218,
      length: 1,
      convRule: rule22
  }, {
      start: 1219,
      length: 1,
      convRule: rule21
  }, {
      start: 1220,
      length: 1,
      convRule: rule22
  }, {
      start: 1221,
      length: 1,
      convRule: rule21
  }, {
      start: 1222,
      length: 1,
      convRule: rule22
  }, {
      start: 1223,
      length: 1,
      convRule: rule21
  }, {
      start: 1224,
      length: 1,
      convRule: rule22
  }, {
      start: 1225,
      length: 1,
      convRule: rule21
  }, {
      start: 1226,
      length: 1,
      convRule: rule22
  }, {
      start: 1227,
      length: 1,
      convRule: rule21
  }, {
      start: 1228,
      length: 1,
      convRule: rule22
  }, {
      start: 1229,
      length: 1,
      convRule: rule21
  }, {
      start: 1230,
      length: 1,
      convRule: rule22
  }, {
      start: 1231,
      length: 1,
      convRule: rule111
  }, {
      start: 1232,
      length: 1,
      convRule: rule21
  }, {
      start: 1233,
      length: 1,
      convRule: rule22
  }, {
      start: 1234,
      length: 1,
      convRule: rule21
  }, {
      start: 1235,
      length: 1,
      convRule: rule22
  }, {
      start: 1236,
      length: 1,
      convRule: rule21
  }, {
      start: 1237,
      length: 1,
      convRule: rule22
  }, {
      start: 1238,
      length: 1,
      convRule: rule21
  }, {
      start: 1239,
      length: 1,
      convRule: rule22
  }, {
      start: 1240,
      length: 1,
      convRule: rule21
  }, {
      start: 1241,
      length: 1,
      convRule: rule22
  }, {
      start: 1242,
      length: 1,
      convRule: rule21
  }, {
      start: 1243,
      length: 1,
      convRule: rule22
  }, {
      start: 1244,
      length: 1,
      convRule: rule21
  }, {
      start: 1245,
      length: 1,
      convRule: rule22
  }, {
      start: 1246,
      length: 1,
      convRule: rule21
  }, {
      start: 1247,
      length: 1,
      convRule: rule22
  }, {
      start: 1248,
      length: 1,
      convRule: rule21
  }, {
      start: 1249,
      length: 1,
      convRule: rule22
  }, {
      start: 1250,
      length: 1,
      convRule: rule21
  }, {
      start: 1251,
      length: 1,
      convRule: rule22
  }, {
      start: 1252,
      length: 1,
      convRule: rule21
  }, {
      start: 1253,
      length: 1,
      convRule: rule22
  }, {
      start: 1254,
      length: 1,
      convRule: rule21
  }, {
      start: 1255,
      length: 1,
      convRule: rule22
  }, {
      start: 1256,
      length: 1,
      convRule: rule21
  }, {
      start: 1257,
      length: 1,
      convRule: rule22
  }, {
      start: 1258,
      length: 1,
      convRule: rule21
  }, {
      start: 1259,
      length: 1,
      convRule: rule22
  }, {
      start: 1260,
      length: 1,
      convRule: rule21
  }, {
      start: 1261,
      length: 1,
      convRule: rule22
  }, {
      start: 1262,
      length: 1,
      convRule: rule21
  }, {
      start: 1263,
      length: 1,
      convRule: rule22
  }, {
      start: 1264,
      length: 1,
      convRule: rule21
  }, {
      start: 1265,
      length: 1,
      convRule: rule22
  }, {
      start: 1266,
      length: 1,
      convRule: rule21
  }, {
      start: 1267,
      length: 1,
      convRule: rule22
  }, {
      start: 1268,
      length: 1,
      convRule: rule21
  }, {
      start: 1269,
      length: 1,
      convRule: rule22
  }, {
      start: 1270,
      length: 1,
      convRule: rule21
  }, {
      start: 1271,
      length: 1,
      convRule: rule22
  }, {
      start: 1272,
      length: 1,
      convRule: rule21
  }, {
      start: 1273,
      length: 1,
      convRule: rule22
  }, {
      start: 1274,
      length: 1,
      convRule: rule21
  }, {
      start: 1275,
      length: 1,
      convRule: rule22
  }, {
      start: 1276,
      length: 1,
      convRule: rule21
  }, {
      start: 1277,
      length: 1,
      convRule: rule22
  }, {
      start: 1278,
      length: 1,
      convRule: rule21
  }, {
      start: 1279,
      length: 1,
      convRule: rule22
  }, {
      start: 1280,
      length: 1,
      convRule: rule21
  }, {
      start: 1281,
      length: 1,
      convRule: rule22
  }, {
      start: 1282,
      length: 1,
      convRule: rule21
  }, {
      start: 1283,
      length: 1,
      convRule: rule22
  }, {
      start: 1284,
      length: 1,
      convRule: rule21
  }, {
      start: 1285,
      length: 1,
      convRule: rule22
  }, {
      start: 1286,
      length: 1,
      convRule: rule21
  }, {
      start: 1287,
      length: 1,
      convRule: rule22
  }, {
      start: 1288,
      length: 1,
      convRule: rule21
  }, {
      start: 1289,
      length: 1,
      convRule: rule22
  }, {
      start: 1290,
      length: 1,
      convRule: rule21
  }, {
      start: 1291,
      length: 1,
      convRule: rule22
  }, {
      start: 1292,
      length: 1,
      convRule: rule21
  }, {
      start: 1293,
      length: 1,
      convRule: rule22
  }, {
      start: 1294,
      length: 1,
      convRule: rule21
  }, {
      start: 1295,
      length: 1,
      convRule: rule22
  }, {
      start: 1296,
      length: 1,
      convRule: rule21
  }, {
      start: 1297,
      length: 1,
      convRule: rule22
  }, {
      start: 1298,
      length: 1,
      convRule: rule21
  }, {
      start: 1299,
      length: 1,
      convRule: rule22
  }, {
      start: 1300,
      length: 1,
      convRule: rule21
  }, {
      start: 1301,
      length: 1,
      convRule: rule22
  }, {
      start: 1302,
      length: 1,
      convRule: rule21
  }, {
      start: 1303,
      length: 1,
      convRule: rule22
  }, {
      start: 1304,
      length: 1,
      convRule: rule21
  }, {
      start: 1305,
      length: 1,
      convRule: rule22
  }, {
      start: 1306,
      length: 1,
      convRule: rule21
  }, {
      start: 1307,
      length: 1,
      convRule: rule22
  }, {
      start: 1308,
      length: 1,
      convRule: rule21
  }, {
      start: 1309,
      length: 1,
      convRule: rule22
  }, {
      start: 1310,
      length: 1,
      convRule: rule21
  }, {
      start: 1311,
      length: 1,
      convRule: rule22
  }, {
      start: 1312,
      length: 1,
      convRule: rule21
  }, {
      start: 1313,
      length: 1,
      convRule: rule22
  }, {
      start: 1314,
      length: 1,
      convRule: rule21
  }, {
      start: 1315,
      length: 1,
      convRule: rule22
  }, {
      start: 1316,
      length: 1,
      convRule: rule21
  }, {
      start: 1317,
      length: 1,
      convRule: rule22
  }, {
      start: 1318,
      length: 1,
      convRule: rule21
  }, {
      start: 1319,
      length: 1,
      convRule: rule22
  }, {
      start: 1329,
      length: 38,
      convRule: rule112
  }, {
      start: 1369,
      length: 1,
      convRule: rule83
  }, {
      start: 1370,
      length: 6,
      convRule: rule2
  }, {
      start: 1377,
      length: 38,
      convRule: rule113
  }, {
      start: 1415,
      length: 1,
      convRule: rule14
  }, {
      start: 1417,
      length: 1,
      convRule: rule2
  }, {
      start: 1418,
      length: 1,
      convRule: rule7
  }, {
      start: 1425,
      length: 45,
      convRule: rule84
  }, {
      start: 1470,
      length: 1,
      convRule: rule7
  }, {
      start: 1471,
      length: 1,
      convRule: rule84
  }, {
      start: 1472,
      length: 1,
      convRule: rule2
  }, {
      start: 1473,
      length: 2,
      convRule: rule84
  }, {
      start: 1475,
      length: 1,
      convRule: rule2
  }, {
      start: 1476,
      length: 2,
      convRule: rule84
  }, {
      start: 1478,
      length: 1,
      convRule: rule2
  }, {
      start: 1479,
      length: 1,
      convRule: rule84
  }, {
      start: 1488,
      length: 27,
      convRule: rule45
  }, {
      start: 1520,
      length: 3,
      convRule: rule45
  }, {
      start: 1523,
      length: 2,
      convRule: rule2
  }, {
      start: 1536,
      length: 4,
      convRule: rule16
  }, {
      start: 1542,
      length: 3,
      convRule: rule6
  }, {
      start: 1545,
      length: 2,
      convRule: rule2
  }, {
      start: 1547,
      length: 1,
      convRule: rule3
  }, {
      start: 1548,
      length: 2,
      convRule: rule2
  }, {
      start: 1550,
      length: 2,
      convRule: rule13
  }, {
      start: 1552,
      length: 11,
      convRule: rule84
  }, {
      start: 1563,
      length: 1,
      convRule: rule2
  }, {
      start: 1566,
      length: 2,
      convRule: rule2
  }, {
      start: 1568,
      length: 32,
      convRule: rule45
  }, {
      start: 1600,
      length: 1,
      convRule: rule83
  }, {
      start: 1601,
      length: 10,
      convRule: rule45
  }, {
      start: 1611,
      length: 21,
      convRule: rule84
  }, {
      start: 1632,
      length: 10,
      convRule: rule8
  }, {
      start: 1642,
      length: 4,
      convRule: rule2
  }, {
      start: 1646,
      length: 2,
      convRule: rule45
  }, {
      start: 1648,
      length: 1,
      convRule: rule84
  }, {
      start: 1649,
      length: 99,
      convRule: rule45
  }, {
      start: 1748,
      length: 1,
      convRule: rule2
  }, {
      start: 1749,
      length: 1,
      convRule: rule45
  }, {
      start: 1750,
      length: 7,
      convRule: rule84
  }, {
      start: 1757,
      length: 1,
      convRule: rule16
  }, {
      start: 1758,
      length: 1,
      convRule: rule13
  }, {
      start: 1759,
      length: 6,
      convRule: rule84
  }, {
      start: 1765,
      length: 2,
      convRule: rule83
  }, {
      start: 1767,
      length: 2,
      convRule: rule84
  }, {
      start: 1769,
      length: 1,
      convRule: rule13
  }, {
      start: 1770,
      length: 4,
      convRule: rule84
  }, {
      start: 1774,
      length: 2,
      convRule: rule45
  }, {
      start: 1776,
      length: 10,
      convRule: rule8
  }, {
      start: 1786,
      length: 3,
      convRule: rule45
  }, {
      start: 1789,
      length: 2,
      convRule: rule13
  }, {
      start: 1791,
      length: 1,
      convRule: rule45
  }, {
      start: 1792,
      length: 14,
      convRule: rule2
  }, {
      start: 1807,
      length: 1,
      convRule: rule16
  }, {
      start: 1808,
      length: 1,
      convRule: rule45
  }, {
      start: 1809,
      length: 1,
      convRule: rule84
  }, {
      start: 1810,
      length: 30,
      convRule: rule45
  }, {
      start: 1840,
      length: 27,
      convRule: rule84
  }, {
      start: 1869,
      length: 89,
      convRule: rule45
  }, {
      start: 1958,
      length: 11,
      convRule: rule84
  }, {
      start: 1969,
      length: 1,
      convRule: rule45
  }, {
      start: 1984,
      length: 10,
      convRule: rule8
  }, {
      start: 1994,
      length: 33,
      convRule: rule45
  }, {
      start: 2027,
      length: 9,
      convRule: rule84
  }, {
      start: 2036,
      length: 2,
      convRule: rule83
  }, {
      start: 2038,
      length: 1,
      convRule: rule13
  }, {
      start: 2039,
      length: 3,
      convRule: rule2
  }, {
      start: 2042,
      length: 1,
      convRule: rule83
  }, {
      start: 2048,
      length: 22,
      convRule: rule45
  }, {
      start: 2070,
      length: 4,
      convRule: rule84
  }, {
      start: 2074,
      length: 1,
      convRule: rule83
  }, {
      start: 2075,
      length: 9,
      convRule: rule84
  }, {
      start: 2084,
      length: 1,
      convRule: rule83
  }, {
      start: 2085,
      length: 3,
      convRule: rule84
  }, {
      start: 2088,
      length: 1,
      convRule: rule83
  }, {
      start: 2089,
      length: 5,
      convRule: rule84
  }, {
      start: 2096,
      length: 15,
      convRule: rule2
  }, {
      start: 2112,
      length: 25,
      convRule: rule45
  }, {
      start: 2137,
      length: 3,
      convRule: rule84
  }, {
      start: 2142,
      length: 1,
      convRule: rule2
  }, {
      start: 2304,
      length: 3,
      convRule: rule84
  }, {
      start: 2307,
      length: 1,
      convRule: rule114
  }, {
      start: 2308,
      length: 54,
      convRule: rule45
  }, {
      start: 2362,
      length: 1,
      convRule: rule84
  }, {
      start: 2363,
      length: 1,
      convRule: rule114
  }, {
      start: 2364,
      length: 1,
      convRule: rule84
  }, {
      start: 2365,
      length: 1,
      convRule: rule45
  }, {
      start: 2366,
      length: 3,
      convRule: rule114
  }, {
      start: 2369,
      length: 8,
      convRule: rule84
  }, {
      start: 2377,
      length: 4,
      convRule: rule114
  }, {
      start: 2381,
      length: 1,
      convRule: rule84
  }, {
      start: 2382,
      length: 2,
      convRule: rule114
  }, {
      start: 2384,
      length: 1,
      convRule: rule45
  }, {
      start: 2385,
      length: 7,
      convRule: rule84
  }, {
      start: 2392,
      length: 10,
      convRule: rule45
  }, {
      start: 2402,
      length: 2,
      convRule: rule84
  }, {
      start: 2404,
      length: 2,
      convRule: rule2
  }, {
      start: 2406,
      length: 10,
      convRule: rule8
  }, {
      start: 2416,
      length: 1,
      convRule: rule2
  }, {
      start: 2417,
      length: 1,
      convRule: rule83
  }, {
      start: 2418,
      length: 6,
      convRule: rule45
  }, {
      start: 2425,
      length: 7,
      convRule: rule45
  }, {
      start: 2433,
      length: 1,
      convRule: rule84
  }, {
      start: 2434,
      length: 2,
      convRule: rule114
  }, {
      start: 2437,
      length: 8,
      convRule: rule45
  }, {
      start: 2447,
      length: 2,
      convRule: rule45
  }, {
      start: 2451,
      length: 22,
      convRule: rule45
  }, {
      start: 2474,
      length: 7,
      convRule: rule45
  }, {
      start: 2482,
      length: 1,
      convRule: rule45
  }, {
      start: 2486,
      length: 4,
      convRule: rule45
  }, {
      start: 2492,
      length: 1,
      convRule: rule84
  }, {
      start: 2493,
      length: 1,
      convRule: rule45
  }, {
      start: 2494,
      length: 3,
      convRule: rule114
  }, {
      start: 2497,
      length: 4,
      convRule: rule84
  }, {
      start: 2503,
      length: 2,
      convRule: rule114
  }, {
      start: 2507,
      length: 2,
      convRule: rule114
  }, {
      start: 2509,
      length: 1,
      convRule: rule84
  }, {
      start: 2510,
      length: 1,
      convRule: rule45
  }, {
      start: 2519,
      length: 1,
      convRule: rule114
  }, {
      start: 2524,
      length: 2,
      convRule: rule45
  }, {
      start: 2527,
      length: 3,
      convRule: rule45
  }, {
      start: 2530,
      length: 2,
      convRule: rule84
  }, {
      start: 2534,
      length: 10,
      convRule: rule8
  }, {
      start: 2544,
      length: 2,
      convRule: rule45
  }, {
      start: 2546,
      length: 2,
      convRule: rule3
  }, {
      start: 2548,
      length: 6,
      convRule: rule17
  }, {
      start: 2554,
      length: 1,
      convRule: rule13
  }, {
      start: 2555,
      length: 1,
      convRule: rule3
  }, {
      start: 2561,
      length: 2,
      convRule: rule84
  }, {
      start: 2563,
      length: 1,
      convRule: rule114
  }, {
      start: 2565,
      length: 6,
      convRule: rule45
  }, {
      start: 2575,
      length: 2,
      convRule: rule45
  }, {
      start: 2579,
      length: 22,
      convRule: rule45
  }, {
      start: 2602,
      length: 7,
      convRule: rule45
  }, {
      start: 2610,
      length: 2,
      convRule: rule45
  }, {
      start: 2613,
      length: 2,
      convRule: rule45
  }, {
      start: 2616,
      length: 2,
      convRule: rule45
  }, {
      start: 2620,
      length: 1,
      convRule: rule84
  }, {
      start: 2622,
      length: 3,
      convRule: rule114
  }, {
      start: 2625,
      length: 2,
      convRule: rule84
  }, {
      start: 2631,
      length: 2,
      convRule: rule84
  }, {
      start: 2635,
      length: 3,
      convRule: rule84
  }, {
      start: 2641,
      length: 1,
      convRule: rule84
  }, {
      start: 2649,
      length: 4,
      convRule: rule45
  }, {
      start: 2654,
      length: 1,
      convRule: rule45
  }, {
      start: 2662,
      length: 10,
      convRule: rule8
  }, {
      start: 2672,
      length: 2,
      convRule: rule84
  }, {
      start: 2674,
      length: 3,
      convRule: rule45
  }, {
      start: 2677,
      length: 1,
      convRule: rule84
  }, {
      start: 2689,
      length: 2,
      convRule: rule84
  }, {
      start: 2691,
      length: 1,
      convRule: rule114
  }, {
      start: 2693,
      length: 9,
      convRule: rule45
  }, {
      start: 2703,
      length: 3,
      convRule: rule45
  }, {
      start: 2707,
      length: 22,
      convRule: rule45
  }, {
      start: 2730,
      length: 7,
      convRule: rule45
  }, {
      start: 2738,
      length: 2,
      convRule: rule45
  }, {
      start: 2741,
      length: 5,
      convRule: rule45
  }, {
      start: 2748,
      length: 1,
      convRule: rule84
  }, {
      start: 2749,
      length: 1,
      convRule: rule45
  }, {
      start: 2750,
      length: 3,
      convRule: rule114
  }, {
      start: 2753,
      length: 5,
      convRule: rule84
  }, {
      start: 2759,
      length: 2,
      convRule: rule84
  }, {
      start: 2761,
      length: 1,
      convRule: rule114
  }, {
      start: 2763,
      length: 2,
      convRule: rule114
  }, {
      start: 2765,
      length: 1,
      convRule: rule84
  }, {
      start: 2768,
      length: 1,
      convRule: rule45
  }, {
      start: 2784,
      length: 2,
      convRule: rule45
  }, {
      start: 2786,
      length: 2,
      convRule: rule84
  }, {
      start: 2790,
      length: 10,
      convRule: rule8
  }, {
      start: 2801,
      length: 1,
      convRule: rule3
  }, {
      start: 2817,
      length: 1,
      convRule: rule84
  }, {
      start: 2818,
      length: 2,
      convRule: rule114
  }, {
      start: 2821,
      length: 8,
      convRule: rule45
  }, {
      start: 2831,
      length: 2,
      convRule: rule45
  }, {
      start: 2835,
      length: 22,
      convRule: rule45
  }, {
      start: 2858,
      length: 7,
      convRule: rule45
  }, {
      start: 2866,
      length: 2,
      convRule: rule45
  }, {
      start: 2869,
      length: 5,
      convRule: rule45
  }, {
      start: 2876,
      length: 1,
      convRule: rule84
  }, {
      start: 2877,
      length: 1,
      convRule: rule45
  }, {
      start: 2878,
      length: 1,
      convRule: rule114
  }, {
      start: 2879,
      length: 1,
      convRule: rule84
  }, {
      start: 2880,
      length: 1,
      convRule: rule114
  }, {
      start: 2881,
      length: 4,
      convRule: rule84
  }, {
      start: 2887,
      length: 2,
      convRule: rule114
  }, {
      start: 2891,
      length: 2,
      convRule: rule114
  }, {
      start: 2893,
      length: 1,
      convRule: rule84
  }, {
      start: 2902,
      length: 1,
      convRule: rule84
  }, {
      start: 2903,
      length: 1,
      convRule: rule114
  }, {
      start: 2908,
      length: 2,
      convRule: rule45
  }, {
      start: 2911,
      length: 3,
      convRule: rule45
  }, {
      start: 2914,
      length: 2,
      convRule: rule84
  }, {
      start: 2918,
      length: 10,
      convRule: rule8
  }, {
      start: 2928,
      length: 1,
      convRule: rule13
  }, {
      start: 2929,
      length: 1,
      convRule: rule45
  }, {
      start: 2930,
      length: 6,
      convRule: rule17
  }, {
      start: 2946,
      length: 1,
      convRule: rule84
  }, {
      start: 2947,
      length: 1,
      convRule: rule45
  }, {
      start: 2949,
      length: 6,
      convRule: rule45
  }, {
      start: 2958,
      length: 3,
      convRule: rule45
  }, {
      start: 2962,
      length: 4,
      convRule: rule45
  }, {
      start: 2969,
      length: 2,
      convRule: rule45
  }, {
      start: 2972,
      length: 1,
      convRule: rule45
  }, {
      start: 2974,
      length: 2,
      convRule: rule45
  }, {
      start: 2979,
      length: 2,
      convRule: rule45
  }, {
      start: 2984,
      length: 3,
      convRule: rule45
  }, {
      start: 2990,
      length: 12,
      convRule: rule45
  }, {
      start: 3006,
      length: 2,
      convRule: rule114
  }, {
      start: 3008,
      length: 1,
      convRule: rule84
  }, {
      start: 3009,
      length: 2,
      convRule: rule114
  }, {
      start: 3014,
      length: 3,
      convRule: rule114
  }, {
      start: 3018,
      length: 3,
      convRule: rule114
  }, {
      start: 3021,
      length: 1,
      convRule: rule84
  }, {
      start: 3024,
      length: 1,
      convRule: rule45
  }, {
      start: 3031,
      length: 1,
      convRule: rule114
  }, {
      start: 3046,
      length: 10,
      convRule: rule8
  }, {
      start: 3056,
      length: 3,
      convRule: rule17
  }, {
      start: 3059,
      length: 6,
      convRule: rule13
  }, {
      start: 3065,
      length: 1,
      convRule: rule3
  }, {
      start: 3066,
      length: 1,
      convRule: rule13
  }, {
      start: 3073,
      length: 3,
      convRule: rule114
  }, {
      start: 3077,
      length: 8,
      convRule: rule45
  }, {
      start: 3086,
      length: 3,
      convRule: rule45
  }, {
      start: 3090,
      length: 23,
      convRule: rule45
  }, {
      start: 3114,
      length: 10,
      convRule: rule45
  }, {
      start: 3125,
      length: 5,
      convRule: rule45
  }, {
      start: 3133,
      length: 1,
      convRule: rule45
  }, {
      start: 3134,
      length: 3,
      convRule: rule84
  }, {
      start: 3137,
      length: 4,
      convRule: rule114
  }, {
      start: 3142,
      length: 3,
      convRule: rule84
  }, {
      start: 3146,
      length: 4,
      convRule: rule84
  }, {
      start: 3157,
      length: 2,
      convRule: rule84
  }, {
      start: 3160,
      length: 2,
      convRule: rule45
  }, {
      start: 3168,
      length: 2,
      convRule: rule45
  }, {
      start: 3170,
      length: 2,
      convRule: rule84
  }, {
      start: 3174,
      length: 10,
      convRule: rule8
  }, {
      start: 3192,
      length: 7,
      convRule: rule17
  }, {
      start: 3199,
      length: 1,
      convRule: rule13
  }, {
      start: 3202,
      length: 2,
      convRule: rule114
  }, {
      start: 3205,
      length: 8,
      convRule: rule45
  }, {
      start: 3214,
      length: 3,
      convRule: rule45
  }, {
      start: 3218,
      length: 23,
      convRule: rule45
  }, {
      start: 3242,
      length: 10,
      convRule: rule45
  }, {
      start: 3253,
      length: 5,
      convRule: rule45
  }, {
      start: 3260,
      length: 1,
      convRule: rule84
  }, {
      start: 3261,
      length: 1,
      convRule: rule45
  }, {
      start: 3262,
      length: 1,
      convRule: rule114
  }, {
      start: 3263,
      length: 1,
      convRule: rule84
  }, {
      start: 3264,
      length: 5,
      convRule: rule114
  }, {
      start: 3270,
      length: 1,
      convRule: rule84
  }, {
      start: 3271,
      length: 2,
      convRule: rule114
  }, {
      start: 3274,
      length: 2,
      convRule: rule114
  }, {
      start: 3276,
      length: 2,
      convRule: rule84
  }, {
      start: 3285,
      length: 2,
      convRule: rule114
  }, {
      start: 3294,
      length: 1,
      convRule: rule45
  }, {
      start: 3296,
      length: 2,
      convRule: rule45
  }, {
      start: 3298,
      length: 2,
      convRule: rule84
  }, {
      start: 3302,
      length: 10,
      convRule: rule8
  }, {
      start: 3313,
      length: 2,
      convRule: rule45
  }, {
      start: 3330,
      length: 2,
      convRule: rule114
  }, {
      start: 3333,
      length: 8,
      convRule: rule45
  }, {
      start: 3342,
      length: 3,
      convRule: rule45
  }, {
      start: 3346,
      length: 41,
      convRule: rule45
  }, {
      start: 3389,
      length: 1,
      convRule: rule45
  }, {
      start: 3390,
      length: 3,
      convRule: rule114
  }, {
      start: 3393,
      length: 4,
      convRule: rule84
  }, {
      start: 3398,
      length: 3,
      convRule: rule114
  }, {
      start: 3402,
      length: 3,
      convRule: rule114
  }, {
      start: 3405,
      length: 1,
      convRule: rule84
  }, {
      start: 3406,
      length: 1,
      convRule: rule45
  }, {
      start: 3415,
      length: 1,
      convRule: rule114
  }, {
      start: 3424,
      length: 2,
      convRule: rule45
  }, {
      start: 3426,
      length: 2,
      convRule: rule84
  }, {
      start: 3430,
      length: 10,
      convRule: rule8
  }, {
      start: 3440,
      length: 6,
      convRule: rule17
  }, {
      start: 3449,
      length: 1,
      convRule: rule13
  }, {
      start: 3450,
      length: 6,
      convRule: rule45
  }, {
      start: 3458,
      length: 2,
      convRule: rule114
  }, {
      start: 3461,
      length: 18,
      convRule: rule45
  }, {
      start: 3482,
      length: 24,
      convRule: rule45
  }, {
      start: 3507,
      length: 9,
      convRule: rule45
  }, {
      start: 3517,
      length: 1,
      convRule: rule45
  }, {
      start: 3520,
      length: 7,
      convRule: rule45
  }, {
      start: 3530,
      length: 1,
      convRule: rule84
  }, {
      start: 3535,
      length: 3,
      convRule: rule114
  }, {
      start: 3538,
      length: 3,
      convRule: rule84
  }, {
      start: 3542,
      length: 1,
      convRule: rule84
  }, {
      start: 3544,
      length: 8,
      convRule: rule114
  }, {
      start: 3570,
      length: 2,
      convRule: rule114
  }, {
      start: 3572,
      length: 1,
      convRule: rule2
  }, {
      start: 3585,
      length: 48,
      convRule: rule45
  }, {
      start: 3633,
      length: 1,
      convRule: rule84
  }, {
      start: 3634,
      length: 2,
      convRule: rule45
  }, {
      start: 3636,
      length: 7,
      convRule: rule84
  }, {
      start: 3647,
      length: 1,
      convRule: rule3
  }, {
      start: 3648,
      length: 6,
      convRule: rule45
  }, {
      start: 3654,
      length: 1,
      convRule: rule83
  }, {
      start: 3655,
      length: 8,
      convRule: rule84
  }, {
      start: 3663,
      length: 1,
      convRule: rule2
  }, {
      start: 3664,
      length: 10,
      convRule: rule8
  }, {
      start: 3674,
      length: 2,
      convRule: rule2
  }, {
      start: 3713,
      length: 2,
      convRule: rule45
  }, {
      start: 3716,
      length: 1,
      convRule: rule45
  }, {
      start: 3719,
      length: 2,
      convRule: rule45
  }, {
      start: 3722,
      length: 1,
      convRule: rule45
  }, {
      start: 3725,
      length: 1,
      convRule: rule45
  }, {
      start: 3732,
      length: 4,
      convRule: rule45
  }, {
      start: 3737,
      length: 7,
      convRule: rule45
  }, {
      start: 3745,
      length: 3,
      convRule: rule45
  }, {
      start: 3749,
      length: 1,
      convRule: rule45
  }, {
      start: 3751,
      length: 1,
      convRule: rule45
  }, {
      start: 3754,
      length: 2,
      convRule: rule45
  }, {
      start: 3757,
      length: 4,
      convRule: rule45
  }, {
      start: 3761,
      length: 1,
      convRule: rule84
  }, {
      start: 3762,
      length: 2,
      convRule: rule45
  }, {
      start: 3764,
      length: 6,
      convRule: rule84
  }, {
      start: 3771,
      length: 2,
      convRule: rule84
  }, {
      start: 3773,
      length: 1,
      convRule: rule45
  }, {
      start: 3776,
      length: 5,
      convRule: rule45
  }, {
      start: 3782,
      length: 1,
      convRule: rule83
  }, {
      start: 3784,
      length: 6,
      convRule: rule84
  }, {
      start: 3792,
      length: 10,
      convRule: rule8
  }, {
      start: 3804,
      length: 2,
      convRule: rule45
  }, {
      start: 3840,
      length: 1,
      convRule: rule45
  }, {
      start: 3841,
      length: 3,
      convRule: rule13
  }, {
      start: 3844,
      length: 15,
      convRule: rule2
  }, {
      start: 3859,
      length: 5,
      convRule: rule13
  }, {
      start: 3864,
      length: 2,
      convRule: rule84
  }, {
      start: 3866,
      length: 6,
      convRule: rule13
  }, {
      start: 3872,
      length: 10,
      convRule: rule8
  }, {
      start: 3882,
      length: 10,
      convRule: rule17
  }, {
      start: 3892,
      length: 1,
      convRule: rule13
  }, {
      start: 3893,
      length: 1,
      convRule: rule84
  }, {
      start: 3894,
      length: 1,
      convRule: rule13
  }, {
      start: 3895,
      length: 1,
      convRule: rule84
  }, {
      start: 3896,
      length: 1,
      convRule: rule13
  }, {
      start: 3897,
      length: 1,
      convRule: rule84
  }, {
      start: 3898,
      length: 1,
      convRule: rule4
  }, {
      start: 3899,
      length: 1,
      convRule: rule5
  }, {
      start: 3900,
      length: 1,
      convRule: rule4
  }, {
      start: 3901,
      length: 1,
      convRule: rule5
  }, {
      start: 3902,
      length: 2,
      convRule: rule114
  }, {
      start: 3904,
      length: 8,
      convRule: rule45
  }, {
      start: 3913,
      length: 36,
      convRule: rule45
  }, {
      start: 3953,
      length: 14,
      convRule: rule84
  }, {
      start: 3967,
      length: 1,
      convRule: rule114
  }, {
      start: 3968,
      length: 5,
      convRule: rule84
  }, {
      start: 3973,
      length: 1,
      convRule: rule2
  }, {
      start: 3974,
      length: 2,
      convRule: rule84
  }, {
      start: 3976,
      length: 5,
      convRule: rule45
  }, {
      start: 3981,
      length: 11,
      convRule: rule84
  }, {
      start: 3993,
      length: 36,
      convRule: rule84
  }, {
      start: 4030,
      length: 8,
      convRule: rule13
  }, {
      start: 4038,
      length: 1,
      convRule: rule84
  }, {
      start: 4039,
      length: 6,
      convRule: rule13
  }, {
      start: 4046,
      length: 2,
      convRule: rule13
  }, {
      start: 4048,
      length: 5,
      convRule: rule2
  }, {
      start: 4053,
      length: 4,
      convRule: rule13
  }, {
      start: 4057,
      length: 2,
      convRule: rule2
  }, {
      start: 4096,
      length: 43,
      convRule: rule45
  }, {
      start: 4139,
      length: 2,
      convRule: rule114
  }, {
      start: 4141,
      length: 4,
      convRule: rule84
  }, {
      start: 4145,
      length: 1,
      convRule: rule114
  }, {
      start: 4146,
      length: 6,
      convRule: rule84
  }, {
      start: 4152,
      length: 1,
      convRule: rule114
  }, {
      start: 4153,
      length: 2,
      convRule: rule84
  }, {
      start: 4155,
      length: 2,
      convRule: rule114
  }, {
      start: 4157,
      length: 2,
      convRule: rule84
  }, {
      start: 4159,
      length: 1,
      convRule: rule45
  }, {
      start: 4160,
      length: 10,
      convRule: rule8
  }, {
      start: 4170,
      length: 6,
      convRule: rule2
  }, {
      start: 4176,
      length: 6,
      convRule: rule45
  }, {
      start: 4182,
      length: 2,
      convRule: rule114
  }, {
      start: 4184,
      length: 2,
      convRule: rule84
  }, {
      start: 4186,
      length: 4,
      convRule: rule45
  }, {
      start: 4190,
      length: 3,
      convRule: rule84
  }, {
      start: 4193,
      length: 1,
      convRule: rule45
  }, {
      start: 4194,
      length: 3,
      convRule: rule114
  }, {
      start: 4197,
      length: 2,
      convRule: rule45
  }, {
      start: 4199,
      length: 7,
      convRule: rule114
  }, {
      start: 4206,
      length: 3,
      convRule: rule45
  }, {
      start: 4209,
      length: 4,
      convRule: rule84
  }, {
      start: 4213,
      length: 13,
      convRule: rule45
  }, {
      start: 4226,
      length: 1,
      convRule: rule84
  }, {
      start: 4227,
      length: 2,
      convRule: rule114
  }, {
      start: 4229,
      length: 2,
      convRule: rule84
  }, {
      start: 4231,
      length: 6,
      convRule: rule114
  }, {
      start: 4237,
      length: 1,
      convRule: rule84
  }, {
      start: 4238,
      length: 1,
      convRule: rule45
  }, {
      start: 4239,
      length: 1,
      convRule: rule114
  }, {
      start: 4240,
      length: 10,
      convRule: rule8
  }, {
      start: 4250,
      length: 3,
      convRule: rule114
  }, {
      start: 4253,
      length: 1,
      convRule: rule84
  }, {
      start: 4254,
      length: 2,
      convRule: rule13
  }, {
      start: 4256,
      length: 38,
      convRule: rule115
  }, {
      start: 4304,
      length: 43,
      convRule: rule45
  }, {
      start: 4347,
      length: 1,
      convRule: rule2
  }, {
      start: 4348,
      length: 1,
      convRule: rule83
  }, {
      start: 4352,
      length: 329,
      convRule: rule45
  }, {
      start: 4682,
      length: 4,
      convRule: rule45
  }, {
      start: 4688,
      length: 7,
      convRule: rule45
  }, {
      start: 4696,
      length: 1,
      convRule: rule45
  }, {
      start: 4698,
      length: 4,
      convRule: rule45
  }, {
      start: 4704,
      length: 41,
      convRule: rule45
  }, {
      start: 4746,
      length: 4,
      convRule: rule45
  }, {
      start: 4752,
      length: 33,
      convRule: rule45
  }, {
      start: 4786,
      length: 4,
      convRule: rule45
  }, {
      start: 4792,
      length: 7,
      convRule: rule45
  }, {
      start: 4800,
      length: 1,
      convRule: rule45
  }, {
      start: 4802,
      length: 4,
      convRule: rule45
  }, {
      start: 4808,
      length: 15,
      convRule: rule45
  }, {
      start: 4824,
      length: 57,
      convRule: rule45
  }, {
      start: 4882,
      length: 4,
      convRule: rule45
  }, {
      start: 4888,
      length: 67,
      convRule: rule45
  }, {
      start: 4957,
      length: 3,
      convRule: rule84
  }, {
      start: 4960,
      length: 1,
      convRule: rule13
  }, {
      start: 4961,
      length: 8,
      convRule: rule2
  }, {
      start: 4969,
      length: 20,
      convRule: rule17
  }, {
      start: 4992,
      length: 16,
      convRule: rule45
  }, {
      start: 5008,
      length: 10,
      convRule: rule13
  }, {
      start: 5024,
      length: 85,
      convRule: rule45
  }, {
      start: 5120,
      length: 1,
      convRule: rule7
  }, {
      start: 5121,
      length: 620,
      convRule: rule45
  }, {
      start: 5741,
      length: 2,
      convRule: rule2
  }, {
      start: 5743,
      length: 17,
      convRule: rule45
  }, {
      start: 5760,
      length: 1,
      convRule: rule1
  }, {
      start: 5761,
      length: 26,
      convRule: rule45
  }, {
      start: 5787,
      length: 1,
      convRule: rule4
  }, {
      start: 5788,
      length: 1,
      convRule: rule5
  }, {
      start: 5792,
      length: 75,
      convRule: rule45
  }, {
      start: 5867,
      length: 3,
      convRule: rule2
  }, {
      start: 5870,
      length: 3,
      convRule: rule116
  }, {
      start: 5888,
      length: 13,
      convRule: rule45
  }, {
      start: 5902,
      length: 4,
      convRule: rule45
  }, {
      start: 5906,
      length: 3,
      convRule: rule84
  }, {
      start: 5920,
      length: 18,
      convRule: rule45
  }, {
      start: 5938,
      length: 3,
      convRule: rule84
  }, {
      start: 5941,
      length: 2,
      convRule: rule2
  }, {
      start: 5952,
      length: 18,
      convRule: rule45
  }, {
      start: 5970,
      length: 2,
      convRule: rule84
  }, {
      start: 5984,
      length: 13,
      convRule: rule45
  }, {
      start: 5998,
      length: 3,
      convRule: rule45
  }, {
      start: 6002,
      length: 2,
      convRule: rule84
  }, {
      start: 6016,
      length: 52,
      convRule: rule45
  }, {
      start: 6068,
      length: 2,
      convRule: rule16
  }, {
      start: 6070,
      length: 1,
      convRule: rule114
  }, {
      start: 6071,
      length: 7,
      convRule: rule84
  }, {
      start: 6078,
      length: 8,
      convRule: rule114
  }, {
      start: 6086,
      length: 1,
      convRule: rule84
  }, {
      start: 6087,
      length: 2,
      convRule: rule114
  }, {
      start: 6089,
      length: 11,
      convRule: rule84
  }, {
      start: 6100,
      length: 3,
      convRule: rule2
  }, {
      start: 6103,
      length: 1,
      convRule: rule83
  }, {
      start: 6104,
      length: 3,
      convRule: rule2
  }, {
      start: 6107,
      length: 1,
      convRule: rule3
  }, {
      start: 6108,
      length: 1,
      convRule: rule45
  }, {
      start: 6109,
      length: 1,
      convRule: rule84
  }, {
      start: 6112,
      length: 10,
      convRule: rule8
  }, {
      start: 6128,
      length: 10,
      convRule: rule17
  }, {
      start: 6144,
      length: 6,
      convRule: rule2
  }, {
      start: 6150,
      length: 1,
      convRule: rule7
  }, {
      start: 6151,
      length: 4,
      convRule: rule2
  }, {
      start: 6155,
      length: 3,
      convRule: rule84
  }, {
      start: 6158,
      length: 1,
      convRule: rule1
  }, {
      start: 6160,
      length: 10,
      convRule: rule8
  }, {
      start: 6176,
      length: 35,
      convRule: rule45
  }, {
      start: 6211,
      length: 1,
      convRule: rule83
  }, {
      start: 6212,
      length: 52,
      convRule: rule45
  }, {
      start: 6272,
      length: 41,
      convRule: rule45
  }, {
      start: 6313,
      length: 1,
      convRule: rule84
  }, {
      start: 6314,
      length: 1,
      convRule: rule45
  }, {
      start: 6320,
      length: 70,
      convRule: rule45
  }, {
      start: 6400,
      length: 29,
      convRule: rule45
  }, {
      start: 6432,
      length: 3,
      convRule: rule84
  }, {
      start: 6435,
      length: 4,
      convRule: rule114
  }, {
      start: 6439,
      length: 2,
      convRule: rule84
  }, {
      start: 6441,
      length: 3,
      convRule: rule114
  }, {
      start: 6448,
      length: 2,
      convRule: rule114
  }, {
      start: 6450,
      length: 1,
      convRule: rule84
  }, {
      start: 6451,
      length: 6,
      convRule: rule114
  }, {
      start: 6457,
      length: 3,
      convRule: rule84
  }, {
      start: 6464,
      length: 1,
      convRule: rule13
  }, {
      start: 6468,
      length: 2,
      convRule: rule2
  }, {
      start: 6470,
      length: 10,
      convRule: rule8
  }, {
      start: 6480,
      length: 30,
      convRule: rule45
  }, {
      start: 6512,
      length: 5,
      convRule: rule45
  }, {
      start: 6528,
      length: 44,
      convRule: rule45
  }, {
      start: 6576,
      length: 17,
      convRule: rule114
  }, {
      start: 6593,
      length: 7,
      convRule: rule45
  }, {
      start: 6600,
      length: 2,
      convRule: rule114
  }, {
      start: 6608,
      length: 10,
      convRule: rule8
  }, {
      start: 6618,
      length: 1,
      convRule: rule17
  }, {
      start: 6622,
      length: 34,
      convRule: rule13
  }, {
      start: 6656,
      length: 23,
      convRule: rule45
  }, {
      start: 6679,
      length: 2,
      convRule: rule84
  }, {
      start: 6681,
      length: 3,
      convRule: rule114
  }, {
      start: 6686,
      length: 2,
      convRule: rule2
  }, {
      start: 6688,
      length: 53,
      convRule: rule45
  }, {
      start: 6741,
      length: 1,
      convRule: rule114
  }, {
      start: 6742,
      length: 1,
      convRule: rule84
  }, {
      start: 6743,
      length: 1,
      convRule: rule114
  }, {
      start: 6744,
      length: 7,
      convRule: rule84
  }, {
      start: 6752,
      length: 1,
      convRule: rule84
  }, {
      start: 6753,
      length: 1,
      convRule: rule114
  }, {
      start: 6754,
      length: 1,
      convRule: rule84
  }, {
      start: 6755,
      length: 2,
      convRule: rule114
  }, {
      start: 6757,
      length: 8,
      convRule: rule84
  }, {
      start: 6765,
      length: 6,
      convRule: rule114
  }, {
      start: 6771,
      length: 10,
      convRule: rule84
  }, {
      start: 6783,
      length: 1,
      convRule: rule84
  }, {
      start: 6784,
      length: 10,
      convRule: rule8
  }, {
      start: 6800,
      length: 10,
      convRule: rule8
  }, {
      start: 6816,
      length: 7,
      convRule: rule2
  }, {
      start: 6823,
      length: 1,
      convRule: rule83
  }, {
      start: 6824,
      length: 6,
      convRule: rule2
  }, {
      start: 6912,
      length: 4,
      convRule: rule84
  }, {
      start: 6916,
      length: 1,
      convRule: rule114
  }, {
      start: 6917,
      length: 47,
      convRule: rule45
  }, {
      start: 6964,
      length: 1,
      convRule: rule84
  }, {
      start: 6965,
      length: 1,
      convRule: rule114
  }, {
      start: 6966,
      length: 5,
      convRule: rule84
  }, {
      start: 6971,
      length: 1,
      convRule: rule114
  }, {
      start: 6972,
      length: 1,
      convRule: rule84
  }, {
      start: 6973,
      length: 5,
      convRule: rule114
  }, {
      start: 6978,
      length: 1,
      convRule: rule84
  }, {
      start: 6979,
      length: 2,
      convRule: rule114
  }, {
      start: 6981,
      length: 7,
      convRule: rule45
  }, {
      start: 6992,
      length: 10,
      convRule: rule8
  }, {
      start: 7002,
      length: 7,
      convRule: rule2
  }, {
      start: 7009,
      length: 10,
      convRule: rule13
  }, {
      start: 7019,
      length: 9,
      convRule: rule84
  }, {
      start: 7028,
      length: 9,
      convRule: rule13
  }, {
      start: 7040,
      length: 2,
      convRule: rule84
  }, {
      start: 7042,
      length: 1,
      convRule: rule114
  }, {
      start: 7043,
      length: 30,
      convRule: rule45
  }, {
      start: 7073,
      length: 1,
      convRule: rule114
  }, {
      start: 7074,
      length: 4,
      convRule: rule84
  }, {
      start: 7078,
      length: 2,
      convRule: rule114
  }, {
      start: 7080,
      length: 2,
      convRule: rule84
  }, {
      start: 7082,
      length: 1,
      convRule: rule114
  }, {
      start: 7086,
      length: 2,
      convRule: rule45
  }, {
      start: 7088,
      length: 10,
      convRule: rule8
  }, {
      start: 7104,
      length: 38,
      convRule: rule45
  }, {
      start: 7142,
      length: 1,
      convRule: rule84
  }, {
      start: 7143,
      length: 1,
      convRule: rule114
  }, {
      start: 7144,
      length: 2,
      convRule: rule84
  }, {
      start: 7146,
      length: 3,
      convRule: rule114
  }, {
      start: 7149,
      length: 1,
      convRule: rule84
  }, {
      start: 7150,
      length: 1,
      convRule: rule114
  }, {
      start: 7151,
      length: 3,
      convRule: rule84
  }, {
      start: 7154,
      length: 2,
      convRule: rule114
  }, {
      start: 7164,
      length: 4,
      convRule: rule2
  }, {
      start: 7168,
      length: 36,
      convRule: rule45
  }, {
      start: 7204,
      length: 8,
      convRule: rule114
  }, {
      start: 7212,
      length: 8,
      convRule: rule84
  }, {
      start: 7220,
      length: 2,
      convRule: rule114
  }, {
      start: 7222,
      length: 2,
      convRule: rule84
  }, {
      start: 7227,
      length: 5,
      convRule: rule2
  }, {
      start: 7232,
      length: 10,
      convRule: rule8
  }, {
      start: 7245,
      length: 3,
      convRule: rule45
  }, {
      start: 7248,
      length: 10,
      convRule: rule8
  }, {
      start: 7258,
      length: 30,
      convRule: rule45
  }, {
      start: 7288,
      length: 6,
      convRule: rule83
  }, {
      start: 7294,
      length: 2,
      convRule: rule2
  }, {
      start: 7376,
      length: 3,
      convRule: rule84
  }, {
      start: 7379,
      length: 1,
      convRule: rule2
  }, {
      start: 7380,
      length: 13,
      convRule: rule84
  }, {
      start: 7393,
      length: 1,
      convRule: rule114
  }, {
      start: 7394,
      length: 7,
      convRule: rule84
  }, {
      start: 7401,
      length: 4,
      convRule: rule45
  }, {
      start: 7405,
      length: 1,
      convRule: rule84
  }, {
      start: 7406,
      length: 4,
      convRule: rule45
  }, {
      start: 7410,
      length: 1,
      convRule: rule114
  }, {
      start: 7424,
      length: 44,
      convRule: rule14
  }, {
      start: 7468,
      length: 54,
      convRule: rule83
  }, {
      start: 7522,
      length: 22,
      convRule: rule14
  }, {
      start: 7544,
      length: 1,
      convRule: rule83
  }, {
      start: 7545,
      length: 1,
      convRule: rule117
  }, {
      start: 7546,
      length: 3,
      convRule: rule14
  }, {
      start: 7549,
      length: 1,
      convRule: rule118
  }, {
      start: 7550,
      length: 29,
      convRule: rule14
  }, {
      start: 7579,
      length: 37,
      convRule: rule83
  }, {
      start: 7616,
      length: 39,
      convRule: rule84
  }, {
      start: 7676,
      length: 4,
      convRule: rule84
  }, {
      start: 7680,
      length: 1,
      convRule: rule21
  }, {
      start: 7681,
      length: 1,
      convRule: rule22
  }, {
      start: 7682,
      length: 1,
      convRule: rule21
  }, {
      start: 7683,
      length: 1,
      convRule: rule22
  }, {
      start: 7684,
      length: 1,
      convRule: rule21
  }, {
      start: 7685,
      length: 1,
      convRule: rule22
  }, {
      start: 7686,
      length: 1,
      convRule: rule21
  }, {
      start: 7687,
      length: 1,
      convRule: rule22
  }, {
      start: 7688,
      length: 1,
      convRule: rule21
  }, {
      start: 7689,
      length: 1,
      convRule: rule22
  }, {
      start: 7690,
      length: 1,
      convRule: rule21
  }, {
      start: 7691,
      length: 1,
      convRule: rule22
  }, {
      start: 7692,
      length: 1,
      convRule: rule21
  }, {
      start: 7693,
      length: 1,
      convRule: rule22
  }, {
      start: 7694,
      length: 1,
      convRule: rule21
  }, {
      start: 7695,
      length: 1,
      convRule: rule22
  }, {
      start: 7696,
      length: 1,
      convRule: rule21
  }, {
      start: 7697,
      length: 1,
      convRule: rule22
  }, {
      start: 7698,
      length: 1,
      convRule: rule21
  }, {
      start: 7699,
      length: 1,
      convRule: rule22
  }, {
      start: 7700,
      length: 1,
      convRule: rule21
  }, {
      start: 7701,
      length: 1,
      convRule: rule22
  }, {
      start: 7702,
      length: 1,
      convRule: rule21
  }, {
      start: 7703,
      length: 1,
      convRule: rule22
  }, {
      start: 7704,
      length: 1,
      convRule: rule21
  }, {
      start: 7705,
      length: 1,
      convRule: rule22
  }, {
      start: 7706,
      length: 1,
      convRule: rule21
  }, {
      start: 7707,
      length: 1,
      convRule: rule22
  }, {
      start: 7708,
      length: 1,
      convRule: rule21
  }, {
      start: 7709,
      length: 1,
      convRule: rule22
  }, {
      start: 7710,
      length: 1,
      convRule: rule21
  }, {
      start: 7711,
      length: 1,
      convRule: rule22
  }, {
      start: 7712,
      length: 1,
      convRule: rule21
  }, {
      start: 7713,
      length: 1,
      convRule: rule22
  }, {
      start: 7714,
      length: 1,
      convRule: rule21
  }, {
      start: 7715,
      length: 1,
      convRule: rule22
  }, {
      start: 7716,
      length: 1,
      convRule: rule21
  }, {
      start: 7717,
      length: 1,
      convRule: rule22
  }, {
      start: 7718,
      length: 1,
      convRule: rule21
  }, {
      start: 7719,
      length: 1,
      convRule: rule22
  }, {
      start: 7720,
      length: 1,
      convRule: rule21
  }, {
      start: 7721,
      length: 1,
      convRule: rule22
  }, {
      start: 7722,
      length: 1,
      convRule: rule21
  }, {
      start: 7723,
      length: 1,
      convRule: rule22
  }, {
      start: 7724,
      length: 1,
      convRule: rule21
  }, {
      start: 7725,
      length: 1,
      convRule: rule22
  }, {
      start: 7726,
      length: 1,
      convRule: rule21
  }, {
      start: 7727,
      length: 1,
      convRule: rule22
  }, {
      start: 7728,
      length: 1,
      convRule: rule21
  }, {
      start: 7729,
      length: 1,
      convRule: rule22
  }, {
      start: 7730,
      length: 1,
      convRule: rule21
  }, {
      start: 7731,
      length: 1,
      convRule: rule22
  }, {
      start: 7732,
      length: 1,
      convRule: rule21
  }, {
      start: 7733,
      length: 1,
      convRule: rule22
  }, {
      start: 7734,
      length: 1,
      convRule: rule21
  }, {
      start: 7735,
      length: 1,
      convRule: rule22
  }, {
      start: 7736,
      length: 1,
      convRule: rule21
  }, {
      start: 7737,
      length: 1,
      convRule: rule22
  }, {
      start: 7738,
      length: 1,
      convRule: rule21
  }, {
      start: 7739,
      length: 1,
      convRule: rule22
  }, {
      start: 7740,
      length: 1,
      convRule: rule21
  }, {
      start: 7741,
      length: 1,
      convRule: rule22
  }, {
      start: 7742,
      length: 1,
      convRule: rule21
  }, {
      start: 7743,
      length: 1,
      convRule: rule22
  }, {
      start: 7744,
      length: 1,
      convRule: rule21
  }, {
      start: 7745,
      length: 1,
      convRule: rule22
  }, {
      start: 7746,
      length: 1,
      convRule: rule21
  }, {
      start: 7747,
      length: 1,
      convRule: rule22
  }, {
      start: 7748,
      length: 1,
      convRule: rule21
  }, {
      start: 7749,
      length: 1,
      convRule: rule22
  }, {
      start: 7750,
      length: 1,
      convRule: rule21
  }, {
      start: 7751,
      length: 1,
      convRule: rule22
  }, {
      start: 7752,
      length: 1,
      convRule: rule21
  }, {
      start: 7753,
      length: 1,
      convRule: rule22
  }, {
      start: 7754,
      length: 1,
      convRule: rule21
  }, {
      start: 7755,
      length: 1,
      convRule: rule22
  }, {
      start: 7756,
      length: 1,
      convRule: rule21
  }, {
      start: 7757,
      length: 1,
      convRule: rule22
  }, {
      start: 7758,
      length: 1,
      convRule: rule21
  }, {
      start: 7759,
      length: 1,
      convRule: rule22
  }, {
      start: 7760,
      length: 1,
      convRule: rule21
  }, {
      start: 7761,
      length: 1,
      convRule: rule22
  }, {
      start: 7762,
      length: 1,
      convRule: rule21
  }, {
      start: 7763,
      length: 1,
      convRule: rule22
  }, {
      start: 7764,
      length: 1,
      convRule: rule21
  }, {
      start: 7765,
      length: 1,
      convRule: rule22
  }, {
      start: 7766,
      length: 1,
      convRule: rule21
  }, {
      start: 7767,
      length: 1,
      convRule: rule22
  }, {
      start: 7768,
      length: 1,
      convRule: rule21
  }, {
      start: 7769,
      length: 1,
      convRule: rule22
  }, {
      start: 7770,
      length: 1,
      convRule: rule21
  }, {
      start: 7771,
      length: 1,
      convRule: rule22
  }, {
      start: 7772,
      length: 1,
      convRule: rule21
  }, {
      start: 7773,
      length: 1,
      convRule: rule22
  }, {
      start: 7774,
      length: 1,
      convRule: rule21
  }, {
      start: 7775,
      length: 1,
      convRule: rule22
  }, {
      start: 7776,
      length: 1,
      convRule: rule21
  }, {
      start: 7777,
      length: 1,
      convRule: rule22
  }, {
      start: 7778,
      length: 1,
      convRule: rule21
  }, {
      start: 7779,
      length: 1,
      convRule: rule22
  }, {
      start: 7780,
      length: 1,
      convRule: rule21
  }, {
      start: 7781,
      length: 1,
      convRule: rule22
  }, {
      start: 7782,
      length: 1,
      convRule: rule21
  }, {
      start: 7783,
      length: 1,
      convRule: rule22
  }, {
      start: 7784,
      length: 1,
      convRule: rule21
  }, {
      start: 7785,
      length: 1,
      convRule: rule22
  }, {
      start: 7786,
      length: 1,
      convRule: rule21
  }, {
      start: 7787,
      length: 1,
      convRule: rule22
  }, {
      start: 7788,
      length: 1,
      convRule: rule21
  }, {
      start: 7789,
      length: 1,
      convRule: rule22
  }, {
      start: 7790,
      length: 1,
      convRule: rule21
  }, {
      start: 7791,
      length: 1,
      convRule: rule22
  }, {
      start: 7792,
      length: 1,
      convRule: rule21
  }, {
      start: 7793,
      length: 1,
      convRule: rule22
  }, {
      start: 7794,
      length: 1,
      convRule: rule21
  }, {
      start: 7795,
      length: 1,
      convRule: rule22
  }, {
      start: 7796,
      length: 1,
      convRule: rule21
  }, {
      start: 7797,
      length: 1,
      convRule: rule22
  }, {
      start: 7798,
      length: 1,
      convRule: rule21
  }, {
      start: 7799,
      length: 1,
      convRule: rule22
  }, {
      start: 7800,
      length: 1,
      convRule: rule21
  }, {
      start: 7801,
      length: 1,
      convRule: rule22
  }, {
      start: 7802,
      length: 1,
      convRule: rule21
  }, {
      start: 7803,
      length: 1,
      convRule: rule22
  }, {
      start: 7804,
      length: 1,
      convRule: rule21
  }, {
      start: 7805,
      length: 1,
      convRule: rule22
  }, {
      start: 7806,
      length: 1,
      convRule: rule21
  }, {
      start: 7807,
      length: 1,
      convRule: rule22
  }, {
      start: 7808,
      length: 1,
      convRule: rule21
  }, {
      start: 7809,
      length: 1,
      convRule: rule22
  }, {
      start: 7810,
      length: 1,
      convRule: rule21
  }, {
      start: 7811,
      length: 1,
      convRule: rule22
  }, {
      start: 7812,
      length: 1,
      convRule: rule21
  }, {
      start: 7813,
      length: 1,
      convRule: rule22
  }, {
      start: 7814,
      length: 1,
      convRule: rule21
  }, {
      start: 7815,
      length: 1,
      convRule: rule22
  }, {
      start: 7816,
      length: 1,
      convRule: rule21
  }, {
      start: 7817,
      length: 1,
      convRule: rule22
  }, {
      start: 7818,
      length: 1,
      convRule: rule21
  }, {
      start: 7819,
      length: 1,
      convRule: rule22
  }, {
      start: 7820,
      length: 1,
      convRule: rule21
  }, {
      start: 7821,
      length: 1,
      convRule: rule22
  }, {
      start: 7822,
      length: 1,
      convRule: rule21
  }, {
      start: 7823,
      length: 1,
      convRule: rule22
  }, {
      start: 7824,
      length: 1,
      convRule: rule21
  }, {
      start: 7825,
      length: 1,
      convRule: rule22
  }, {
      start: 7826,
      length: 1,
      convRule: rule21
  }, {
      start: 7827,
      length: 1,
      convRule: rule22
  }, {
      start: 7828,
      length: 1,
      convRule: rule21
  }, {
      start: 7829,
      length: 1,
      convRule: rule22
  }, {
      start: 7830,
      length: 5,
      convRule: rule14
  }, {
      start: 7835,
      length: 1,
      convRule: rule119
  }, {
      start: 7836,
      length: 2,
      convRule: rule14
  }, {
      start: 7838,
      length: 1,
      convRule: rule120
  }, {
      start: 7839,
      length: 1,
      convRule: rule14
  }, {
      start: 7840,
      length: 1,
      convRule: rule21
  }, {
      start: 7841,
      length: 1,
      convRule: rule22
  }, {
      start: 7842,
      length: 1,
      convRule: rule21
  }, {
      start: 7843,
      length: 1,
      convRule: rule22
  }, {
      start: 7844,
      length: 1,
      convRule: rule21
  }, {
      start: 7845,
      length: 1,
      convRule: rule22
  }, {
      start: 7846,
      length: 1,
      convRule: rule21
  }, {
      start: 7847,
      length: 1,
      convRule: rule22
  }, {
      start: 7848,
      length: 1,
      convRule: rule21
  }, {
      start: 7849,
      length: 1,
      convRule: rule22
  }, {
      start: 7850,
      length: 1,
      convRule: rule21
  }, {
      start: 7851,
      length: 1,
      convRule: rule22
  }, {
      start: 7852,
      length: 1,
      convRule: rule21
  }, {
      start: 7853,
      length: 1,
      convRule: rule22
  }, {
      start: 7854,
      length: 1,
      convRule: rule21
  }, {
      start: 7855,
      length: 1,
      convRule: rule22
  }, {
      start: 7856,
      length: 1,
      convRule: rule21
  }, {
      start: 7857,
      length: 1,
      convRule: rule22
  }, {
      start: 7858,
      length: 1,
      convRule: rule21
  }, {
      start: 7859,
      length: 1,
      convRule: rule22
  }, {
      start: 7860,
      length: 1,
      convRule: rule21
  }, {
      start: 7861,
      length: 1,
      convRule: rule22
  }, {
      start: 7862,
      length: 1,
      convRule: rule21
  }, {
      start: 7863,
      length: 1,
      convRule: rule22
  }, {
      start: 7864,
      length: 1,
      convRule: rule21
  }, {
      start: 7865,
      length: 1,
      convRule: rule22
  }, {
      start: 7866,
      length: 1,
      convRule: rule21
  }, {
      start: 7867,
      length: 1,
      convRule: rule22
  }, {
      start: 7868,
      length: 1,
      convRule: rule21
  }, {
      start: 7869,
      length: 1,
      convRule: rule22
  }, {
      start: 7870,
      length: 1,
      convRule: rule21
  }, {
      start: 7871,
      length: 1,
      convRule: rule22
  }, {
      start: 7872,
      length: 1,
      convRule: rule21
  }, {
      start: 7873,
      length: 1,
      convRule: rule22
  }, {
      start: 7874,
      length: 1,
      convRule: rule21
  }, {
      start: 7875,
      length: 1,
      convRule: rule22
  }, {
      start: 7876,
      length: 1,
      convRule: rule21
  }, {
      start: 7877,
      length: 1,
      convRule: rule22
  }, {
      start: 7878,
      length: 1,
      convRule: rule21
  }, {
      start: 7879,
      length: 1,
      convRule: rule22
  }, {
      start: 7880,
      length: 1,
      convRule: rule21
  }, {
      start: 7881,
      length: 1,
      convRule: rule22
  }, {
      start: 7882,
      length: 1,
      convRule: rule21
  }, {
      start: 7883,
      length: 1,
      convRule: rule22
  }, {
      start: 7884,
      length: 1,
      convRule: rule21
  }, {
      start: 7885,
      length: 1,
      convRule: rule22
  }, {
      start: 7886,
      length: 1,
      convRule: rule21
  }, {
      start: 7887,
      length: 1,
      convRule: rule22
  }, {
      start: 7888,
      length: 1,
      convRule: rule21
  }, {
      start: 7889,
      length: 1,
      convRule: rule22
  }, {
      start: 7890,
      length: 1,
      convRule: rule21
  }, {
      start: 7891,
      length: 1,
      convRule: rule22
  }, {
      start: 7892,
      length: 1,
      convRule: rule21
  }, {
      start: 7893,
      length: 1,
      convRule: rule22
  }, {
      start: 7894,
      length: 1,
      convRule: rule21
  }, {
      start: 7895,
      length: 1,
      convRule: rule22
  }, {
      start: 7896,
      length: 1,
      convRule: rule21
  }, {
      start: 7897,
      length: 1,
      convRule: rule22
  }, {
      start: 7898,
      length: 1,
      convRule: rule21
  }, {
      start: 7899,
      length: 1,
      convRule: rule22
  }, {
      start: 7900,
      length: 1,
      convRule: rule21
  }, {
      start: 7901,
      length: 1,
      convRule: rule22
  }, {
      start: 7902,
      length: 1,
      convRule: rule21
  }, {
      start: 7903,
      length: 1,
      convRule: rule22
  }, {
      start: 7904,
      length: 1,
      convRule: rule21
  }, {
      start: 7905,
      length: 1,
      convRule: rule22
  }, {
      start: 7906,
      length: 1,
      convRule: rule21
  }, {
      start: 7907,
      length: 1,
      convRule: rule22
  }, {
      start: 7908,
      length: 1,
      convRule: rule21
  }, {
      start: 7909,
      length: 1,
      convRule: rule22
  }, {
      start: 7910,
      length: 1,
      convRule: rule21
  }, {
      start: 7911,
      length: 1,
      convRule: rule22
  }, {
      start: 7912,
      length: 1,
      convRule: rule21
  }, {
      start: 7913,
      length: 1,
      convRule: rule22
  }, {
      start: 7914,
      length: 1,
      convRule: rule21
  }, {
      start: 7915,
      length: 1,
      convRule: rule22
  }, {
      start: 7916,
      length: 1,
      convRule: rule21
  }, {
      start: 7917,
      length: 1,
      convRule: rule22
  }, {
      start: 7918,
      length: 1,
      convRule: rule21
  }, {
      start: 7919,
      length: 1,
      convRule: rule22
  }, {
      start: 7920,
      length: 1,
      convRule: rule21
  }, {
      start: 7921,
      length: 1,
      convRule: rule22
  }, {
      start: 7922,
      length: 1,
      convRule: rule21
  }, {
      start: 7923,
      length: 1,
      convRule: rule22
  }, {
      start: 7924,
      length: 1,
      convRule: rule21
  }, {
      start: 7925,
      length: 1,
      convRule: rule22
  }, {
      start: 7926,
      length: 1,
      convRule: rule21
  }, {
      start: 7927,
      length: 1,
      convRule: rule22
  }, {
      start: 7928,
      length: 1,
      convRule: rule21
  }, {
      start: 7929,
      length: 1,
      convRule: rule22
  }, {
      start: 7930,
      length: 1,
      convRule: rule21
  }, {
      start: 7931,
      length: 1,
      convRule: rule22
  }, {
      start: 7932,
      length: 1,
      convRule: rule21
  }, {
      start: 7933,
      length: 1,
      convRule: rule22
  }, {
      start: 7934,
      length: 1,
      convRule: rule21
  }, {
      start: 7935,
      length: 1,
      convRule: rule22
  }, {
      start: 7936,
      length: 8,
      convRule: rule121
  }, {
      start: 7944,
      length: 8,
      convRule: rule122
  }, {
      start: 7952,
      length: 6,
      convRule: rule121
  }, {
      start: 7960,
      length: 6,
      convRule: rule122
  }, {
      start: 7968,
      length: 8,
      convRule: rule121
  }, {
      start: 7976,
      length: 8,
      convRule: rule122
  }, {
      start: 7984,
      length: 8,
      convRule: rule121
  }, {
      start: 7992,
      length: 8,
      convRule: rule122
  }, {
      start: 8000,
      length: 6,
      convRule: rule121
  }, {
      start: 8008,
      length: 6,
      convRule: rule122
  }, {
      start: 8016,
      length: 1,
      convRule: rule14
  }, {
      start: 8017,
      length: 1,
      convRule: rule121
  }, {
      start: 8018,
      length: 1,
      convRule: rule14
  }, {
      start: 8019,
      length: 1,
      convRule: rule121
  }, {
      start: 8020,
      length: 1,
      convRule: rule14
  }, {
      start: 8021,
      length: 1,
      convRule: rule121
  }, {
      start: 8022,
      length: 1,
      convRule: rule14
  }, {
      start: 8023,
      length: 1,
      convRule: rule121
  }, {
      start: 8025,
      length: 1,
      convRule: rule122
  }, {
      start: 8027,
      length: 1,
      convRule: rule122
  }, {
      start: 8029,
      length: 1,
      convRule: rule122
  }, {
      start: 8031,
      length: 1,
      convRule: rule122
  }, {
      start: 8032,
      length: 8,
      convRule: rule121
  }, {
      start: 8040,
      length: 8,
      convRule: rule122
  }, {
      start: 8048,
      length: 2,
      convRule: rule123
  }, {
      start: 8050,
      length: 4,
      convRule: rule124
  }, {
      start: 8054,
      length: 2,
      convRule: rule125
  }, {
      start: 8056,
      length: 2,
      convRule: rule126
  }, {
      start: 8058,
      length: 2,
      convRule: rule127
  }, {
      start: 8060,
      length: 2,
      convRule: rule128
  }, {
      start: 8064,
      length: 8,
      convRule: rule121
  }, {
      start: 8072,
      length: 8,
      convRule: rule129
  }, {
      start: 8080,
      length: 8,
      convRule: rule121
  }, {
      start: 8088,
      length: 8,
      convRule: rule129
  }, {
      start: 8096,
      length: 8,
      convRule: rule121
  }, {
      start: 8104,
      length: 8,
      convRule: rule129
  }, {
      start: 8112,
      length: 2,
      convRule: rule121
  }, {
      start: 8114,
      length: 1,
      convRule: rule14
  }, {
      start: 8115,
      length: 1,
      convRule: rule130
  }, {
      start: 8116,
      length: 1,
      convRule: rule14
  }, {
      start: 8118,
      length: 2,
      convRule: rule14
  }, {
      start: 8120,
      length: 2,
      convRule: rule122
  }, {
      start: 8122,
      length: 2,
      convRule: rule131
  }, {
      start: 8124,
      length: 1,
      convRule: rule132
  }, {
      start: 8125,
      length: 1,
      convRule: rule10
  }, {
      start: 8126,
      length: 1,
      convRule: rule133
  }, {
      start: 8127,
      length: 3,
      convRule: rule10
  }, {
      start: 8130,
      length: 1,
      convRule: rule14
  }, {
      start: 8131,
      length: 1,
      convRule: rule130
  }, {
      start: 8132,
      length: 1,
      convRule: rule14
  }, {
      start: 8134,
      length: 2,
      convRule: rule14
  }, {
      start: 8136,
      length: 4,
      convRule: rule134
  }, {
      start: 8140,
      length: 1,
      convRule: rule132
  }, {
      start: 8141,
      length: 3,
      convRule: rule10
  }, {
      start: 8144,
      length: 2,
      convRule: rule121
  }, {
      start: 8146,
      length: 2,
      convRule: rule14
  }, {
      start: 8150,
      length: 2,
      convRule: rule14
  }, {
      start: 8152,
      length: 2,
      convRule: rule122
  }, {
      start: 8154,
      length: 2,
      convRule: rule135
  }, {
      start: 8157,
      length: 3,
      convRule: rule10
  }, {
      start: 8160,
      length: 2,
      convRule: rule121
  }, {
      start: 8162,
      length: 3,
      convRule: rule14
  }, {
      start: 8165,
      length: 1,
      convRule: rule104
  }, {
      start: 8166,
      length: 2,
      convRule: rule14
  }, {
      start: 8168,
      length: 2,
      convRule: rule122
  }, {
      start: 8170,
      length: 2,
      convRule: rule136
  }, {
      start: 8172,
      length: 1,
      convRule: rule107
  }, {
      start: 8173,
      length: 3,
      convRule: rule10
  }, {
      start: 8178,
      length: 1,
      convRule: rule14
  }, {
      start: 8179,
      length: 1,
      convRule: rule130
  }, {
      start: 8180,
      length: 1,
      convRule: rule14
  }, {
      start: 8182,
      length: 2,
      convRule: rule14
  }, {
      start: 8184,
      length: 2,
      convRule: rule137
  }, {
      start: 8186,
      length: 2,
      convRule: rule138
  }, {
      start: 8188,
      length: 1,
      convRule: rule132
  }, {
      start: 8189,
      length: 2,
      convRule: rule10
  }, {
      start: 8192,
      length: 11,
      convRule: rule1
  }, {
      start: 8203,
      length: 5,
      convRule: rule16
  }, {
      start: 8208,
      length: 6,
      convRule: rule7
  }, {
      start: 8214,
      length: 2,
      convRule: rule2
  }, {
      start: 8216,
      length: 1,
      convRule: rule15
  }, {
      start: 8217,
      length: 1,
      convRule: rule19
  }, {
      start: 8218,
      length: 1,
      convRule: rule4
  }, {
      start: 8219,
      length: 2,
      convRule: rule15
  }, {
      start: 8221,
      length: 1,
      convRule: rule19
  }, {
      start: 8222,
      length: 1,
      convRule: rule4
  }, {
      start: 8223,
      length: 1,
      convRule: rule15
  }, {
      start: 8224,
      length: 8,
      convRule: rule2
  }, {
      start: 8232,
      length: 1,
      convRule: rule139
  }, {
      start: 8233,
      length: 1,
      convRule: rule140
  }, {
      start: 8234,
      length: 5,
      convRule: rule16
  }, {
      start: 8239,
      length: 1,
      convRule: rule1
  }, {
      start: 8240,
      length: 9,
      convRule: rule2
  }, {
      start: 8249,
      length: 1,
      convRule: rule15
  }, {
      start: 8250,
      length: 1,
      convRule: rule19
  }, {
      start: 8251,
      length: 4,
      convRule: rule2
  }, {
      start: 8255,
      length: 2,
      convRule: rule11
  }, {
      start: 8257,
      length: 3,
      convRule: rule2
  }, {
      start: 8260,
      length: 1,
      convRule: rule6
  }, {
      start: 8261,
      length: 1,
      convRule: rule4
  }, {
      start: 8262,
      length: 1,
      convRule: rule5
  }, {
      start: 8263,
      length: 11,
      convRule: rule2
  }, {
      start: 8274,
      length: 1,
      convRule: rule6
  }, {
      start: 8275,
      length: 1,
      convRule: rule2
  }, {
      start: 8276,
      length: 1,
      convRule: rule11
  }, {
      start: 8277,
      length: 10,
      convRule: rule2
  }, {
      start: 8287,
      length: 1,
      convRule: rule1
  }, {
      start: 8288,
      length: 5,
      convRule: rule16
  }, {
      start: 8298,
      length: 6,
      convRule: rule16
  }, {
      start: 8304,
      length: 1,
      convRule: rule17
  }, {
      start: 8305,
      length: 1,
      convRule: rule83
  }, {
      start: 8308,
      length: 6,
      convRule: rule17
  }, {
      start: 8314,
      length: 3,
      convRule: rule6
  }, {
      start: 8317,
      length: 1,
      convRule: rule4
  }, {
      start: 8318,
      length: 1,
      convRule: rule5
  }, {
      start: 8319,
      length: 1,
      convRule: rule83
  }, {
      start: 8320,
      length: 10,
      convRule: rule17
  }, {
      start: 8330,
      length: 3,
      convRule: rule6
  }, {
      start: 8333,
      length: 1,
      convRule: rule4
  }, {
      start: 8334,
      length: 1,
      convRule: rule5
  }, {
      start: 8336,
      length: 13,
      convRule: rule83
  }, {
      start: 8352,
      length: 26,
      convRule: rule3
  }, {
      start: 8400,
      length: 13,
      convRule: rule84
  }, {
      start: 8413,
      length: 4,
      convRule: rule109
  }, {
      start: 8417,
      length: 1,
      convRule: rule84
  }, {
      start: 8418,
      length: 3,
      convRule: rule109
  }, {
      start: 8421,
      length: 12,
      convRule: rule84
  }, {
      start: 8448,
      length: 2,
      convRule: rule13
  }, {
      start: 8450,
      length: 1,
      convRule: rule98
  }, {
      start: 8451,
      length: 4,
      convRule: rule13
  }, {
      start: 8455,
      length: 1,
      convRule: rule98
  }, {
      start: 8456,
      length: 2,
      convRule: rule13
  }, {
      start: 8458,
      length: 1,
      convRule: rule14
  }, {
      start: 8459,
      length: 3,
      convRule: rule98
  }, {
      start: 8462,
      length: 2,
      convRule: rule14
  }, {
      start: 8464,
      length: 3,
      convRule: rule98
  }, {
      start: 8467,
      length: 1,
      convRule: rule14
  }, {
      start: 8468,
      length: 1,
      convRule: rule13
  }, {
      start: 8469,
      length: 1,
      convRule: rule98
  }, {
      start: 8470,
      length: 2,
      convRule: rule13
  }, {
      start: 8472,
      length: 1,
      convRule: rule6
  }, {
      start: 8473,
      length: 5,
      convRule: rule98
  }, {
      start: 8478,
      length: 6,
      convRule: rule13
  }, {
      start: 8484,
      length: 1,
      convRule: rule98
  }, {
      start: 8485,
      length: 1,
      convRule: rule13
  }, {
      start: 8486,
      length: 1,
      convRule: rule141
  }, {
      start: 8487,
      length: 1,
      convRule: rule13
  }, {
      start: 8488,
      length: 1,
      convRule: rule98
  }, {
      start: 8489,
      length: 1,
      convRule: rule13
  }, {
      start: 8490,
      length: 1,
      convRule: rule142
  }, {
      start: 8491,
      length: 1,
      convRule: rule143
  }, {
      start: 8492,
      length: 2,
      convRule: rule98
  }, {
      start: 8494,
      length: 1,
      convRule: rule13
  }, {
      start: 8495,
      length: 1,
      convRule: rule14
  }, {
      start: 8496,
      length: 2,
      convRule: rule98
  }, {
      start: 8498,
      length: 1,
      convRule: rule144
  }, {
      start: 8499,
      length: 1,
      convRule: rule98
  }, {
      start: 8500,
      length: 1,
      convRule: rule14
  }, {
      start: 8501,
      length: 4,
      convRule: rule45
  }, {
      start: 8505,
      length: 1,
      convRule: rule14
  }, {
      start: 8506,
      length: 2,
      convRule: rule13
  }, {
      start: 8508,
      length: 2,
      convRule: rule14
  }, {
      start: 8510,
      length: 2,
      convRule: rule98
  }, {
      start: 8512,
      length: 5,
      convRule: rule6
  }, {
      start: 8517,
      length: 1,
      convRule: rule98
  }, {
      start: 8518,
      length: 4,
      convRule: rule14
  }, {
      start: 8522,
      length: 1,
      convRule: rule13
  }, {
      start: 8523,
      length: 1,
      convRule: rule6
  }, {
      start: 8524,
      length: 2,
      convRule: rule13
  }, {
      start: 8526,
      length: 1,
      convRule: rule145
  }, {
      start: 8527,
      length: 1,
      convRule: rule13
  }, {
      start: 8528,
      length: 16,
      convRule: rule17
  }, {
      start: 8544,
      length: 16,
      convRule: rule146
  }, {
      start: 8560,
      length: 16,
      convRule: rule147
  }, {
      start: 8576,
      length: 3,
      convRule: rule116
  }, {
      start: 8579,
      length: 1,
      convRule: rule21
  }, {
      start: 8580,
      length: 1,
      convRule: rule22
  }, {
      start: 8581,
      length: 4,
      convRule: rule116
  }, {
      start: 8585,
      length: 1,
      convRule: rule17
  }, {
      start: 8592,
      length: 5,
      convRule: rule6
  }, {
      start: 8597,
      length: 5,
      convRule: rule13
  }, {
      start: 8602,
      length: 2,
      convRule: rule6
  }, {
      start: 8604,
      length: 4,
      convRule: rule13
  }, {
      start: 8608,
      length: 1,
      convRule: rule6
  }, {
      start: 8609,
      length: 2,
      convRule: rule13
  }, {
      start: 8611,
      length: 1,
      convRule: rule6
  }, {
      start: 8612,
      length: 2,
      convRule: rule13
  }, {
      start: 8614,
      length: 1,
      convRule: rule6
  }, {
      start: 8615,
      length: 7,
      convRule: rule13
  }, {
      start: 8622,
      length: 1,
      convRule: rule6
  }, {
      start: 8623,
      length: 31,
      convRule: rule13
  }, {
      start: 8654,
      length: 2,
      convRule: rule6
  }, {
      start: 8656,
      length: 2,
      convRule: rule13
  }, {
      start: 8658,
      length: 1,
      convRule: rule6
  }, {
      start: 8659,
      length: 1,
      convRule: rule13
  }, {
      start: 8660,
      length: 1,
      convRule: rule6
  }, {
      start: 8661,
      length: 31,
      convRule: rule13
  }, {
      start: 8692,
      length: 268,
      convRule: rule6
  }, {
      start: 8960,
      length: 8,
      convRule: rule13
  }, {
      start: 8968,
      length: 4,
      convRule: rule6
  }, {
      start: 8972,
      length: 20,
      convRule: rule13
  }, {
      start: 8992,
      length: 2,
      convRule: rule6
  }, {
      start: 8994,
      length: 7,
      convRule: rule13
  }, {
      start: 9001,
      length: 1,
      convRule: rule4
  }, {
      start: 9002,
      length: 1,
      convRule: rule5
  }, {
      start: 9003,
      length: 81,
      convRule: rule13
  }, {
      start: 9084,
      length: 1,
      convRule: rule6
  }, {
      start: 9085,
      length: 30,
      convRule: rule13
  }, {
      start: 9115,
      length: 25,
      convRule: rule6
  }, {
      start: 9140,
      length: 40,
      convRule: rule13
  }, {
      start: 9180,
      length: 6,
      convRule: rule6
  }, {
      start: 9186,
      length: 18,
      convRule: rule13
  }, {
      start: 9216,
      length: 39,
      convRule: rule13
  }, {
      start: 9280,
      length: 11,
      convRule: rule13
  }, {
      start: 9312,
      length: 60,
      convRule: rule17
  }, {
      start: 9372,
      length: 26,
      convRule: rule13
  }, {
      start: 9398,
      length: 26,
      convRule: rule148
  }, {
      start: 9424,
      length: 26,
      convRule: rule149
  }, {
      start: 9450,
      length: 22,
      convRule: rule17
  }, {
      start: 9472,
      length: 183,
      convRule: rule13
  }, {
      start: 9655,
      length: 1,
      convRule: rule6
  }, {
      start: 9656,
      length: 9,
      convRule: rule13
  }, {
      start: 9665,
      length: 1,
      convRule: rule6
  }, {
      start: 9666,
      length: 54,
      convRule: rule13
  }, {
      start: 9720,
      length: 8,
      convRule: rule6
  }, {
      start: 9728,
      length: 111,
      convRule: rule13
  }, {
      start: 9839,
      length: 1,
      convRule: rule6
  }, {
      start: 9840,
      length: 144,
      convRule: rule13
  }, {
      start: 9985,
      length: 103,
      convRule: rule13
  }, {
      start: 10088,
      length: 1,
      convRule: rule4
  }, {
      start: 10089,
      length: 1,
      convRule: rule5
  }, {
      start: 10090,
      length: 1,
      convRule: rule4
  }, {
      start: 10091,
      length: 1,
      convRule: rule5
  }, {
      start: 10092,
      length: 1,
      convRule: rule4
  }, {
      start: 10093,
      length: 1,
      convRule: rule5
  }, {
      start: 10094,
      length: 1,
      convRule: rule4
  }, {
      start: 10095,
      length: 1,
      convRule: rule5
  }, {
      start: 10096,
      length: 1,
      convRule: rule4
  }, {
      start: 10097,
      length: 1,
      convRule: rule5
  }, {
      start: 10098,
      length: 1,
      convRule: rule4
  }, {
      start: 10099,
      length: 1,
      convRule: rule5
  }, {
      start: 10100,
      length: 1,
      convRule: rule4
  }, {
      start: 10101,
      length: 1,
      convRule: rule5
  }, {
      start: 10102,
      length: 30,
      convRule: rule17
  }, {
      start: 10132,
      length: 44,
      convRule: rule13
  }, {
      start: 10176,
      length: 5,
      convRule: rule6
  }, {
      start: 10181,
      length: 1,
      convRule: rule4
  }, {
      start: 10182,
      length: 1,
      convRule: rule5
  }, {
      start: 10183,
      length: 4,
      convRule: rule6
  }, {
      start: 10188,
      length: 1,
      convRule: rule6
  }, {
      start: 10190,
      length: 24,
      convRule: rule6
  }, {
      start: 10214,
      length: 1,
      convRule: rule4
  }, {
      start: 10215,
      length: 1,
      convRule: rule5
  }, {
      start: 10216,
      length: 1,
      convRule: rule4
  }, {
      start: 10217,
      length: 1,
      convRule: rule5
  }, {
      start: 10218,
      length: 1,
      convRule: rule4
  }, {
      start: 10219,
      length: 1,
      convRule: rule5
  }, {
      start: 10220,
      length: 1,
      convRule: rule4
  }, {
      start: 10221,
      length: 1,
      convRule: rule5
  }, {
      start: 10222,
      length: 1,
      convRule: rule4
  }, {
      start: 10223,
      length: 1,
      convRule: rule5
  }, {
      start: 10224,
      length: 16,
      convRule: rule6
  }, {
      start: 10240,
      length: 256,
      convRule: rule13
  }, {
      start: 10496,
      length: 131,
      convRule: rule6
  }, {
      start: 10627,
      length: 1,
      convRule: rule4
  }, {
      start: 10628,
      length: 1,
      convRule: rule5
  }, {
      start: 10629,
      length: 1,
      convRule: rule4
  }, {
      start: 10630,
      length: 1,
      convRule: rule5
  }, {
      start: 10631,
      length: 1,
      convRule: rule4
  }, {
      start: 10632,
      length: 1,
      convRule: rule5
  }, {
      start: 10633,
      length: 1,
      convRule: rule4
  }, {
      start: 10634,
      length: 1,
      convRule: rule5
  }, {
      start: 10635,
      length: 1,
      convRule: rule4
  }, {
      start: 10636,
      length: 1,
      convRule: rule5
  }, {
      start: 10637,
      length: 1,
      convRule: rule4
  }, {
      start: 10638,
      length: 1,
      convRule: rule5
  }, {
      start: 10639,
      length: 1,
      convRule: rule4
  }, {
      start: 10640,
      length: 1,
      convRule: rule5
  }, {
      start: 10641,
      length: 1,
      convRule: rule4
  }, {
      start: 10642,
      length: 1,
      convRule: rule5
  }, {
      start: 10643,
      length: 1,
      convRule: rule4
  }, {
      start: 10644,
      length: 1,
      convRule: rule5
  }, {
      start: 10645,
      length: 1,
      convRule: rule4
  }, {
      start: 10646,
      length: 1,
      convRule: rule5
  }, {
      start: 10647,
      length: 1,
      convRule: rule4
  }, {
      start: 10648,
      length: 1,
      convRule: rule5
  }, {
      start: 10649,
      length: 63,
      convRule: rule6
  }, {
      start: 10712,
      length: 1,
      convRule: rule4
  }, {
      start: 10713,
      length: 1,
      convRule: rule5
  }, {
      start: 10714,
      length: 1,
      convRule: rule4
  }, {
      start: 10715,
      length: 1,
      convRule: rule5
  }, {
      start: 10716,
      length: 32,
      convRule: rule6
  }, {
      start: 10748,
      length: 1,
      convRule: rule4
  }, {
      start: 10749,
      length: 1,
      convRule: rule5
  }, {
      start: 10750,
      length: 258,
      convRule: rule6
  }, {
      start: 11008,
      length: 48,
      convRule: rule13
  }, {
      start: 11056,
      length: 21,
      convRule: rule6
  }, {
      start: 11077,
      length: 2,
      convRule: rule13
  }, {
      start: 11079,
      length: 6,
      convRule: rule6
  }, {
      start: 11088,
      length: 10,
      convRule: rule13
  }, {
      start: 11264,
      length: 47,
      convRule: rule112
  }, {
      start: 11312,
      length: 47,
      convRule: rule113
  }, {
      start: 11360,
      length: 1,
      convRule: rule21
  }, {
      start: 11361,
      length: 1,
      convRule: rule22
  }, {
      start: 11362,
      length: 1,
      convRule: rule150
  }, {
      start: 11363,
      length: 1,
      convRule: rule151
  }, {
      start: 11364,
      length: 1,
      convRule: rule152
  }, {
      start: 11365,
      length: 1,
      convRule: rule153
  }, {
      start: 11366,
      length: 1,
      convRule: rule154
  }, {
      start: 11367,
      length: 1,
      convRule: rule21
  }, {
      start: 11368,
      length: 1,
      convRule: rule22
  }, {
      start: 11369,
      length: 1,
      convRule: rule21
  }, {
      start: 11370,
      length: 1,
      convRule: rule22
  }, {
      start: 11371,
      length: 1,
      convRule: rule21
  }, {
      start: 11372,
      length: 1,
      convRule: rule22
  }, {
      start: 11373,
      length: 1,
      convRule: rule155
  }, {
      start: 11374,
      length: 1,
      convRule: rule156
  }, {
      start: 11375,
      length: 1,
      convRule: rule157
  }, {
      start: 11376,
      length: 1,
      convRule: rule158
  }, {
      start: 11377,
      length: 1,
      convRule: rule14
  }, {
      start: 11378,
      length: 1,
      convRule: rule21
  }, {
      start: 11379,
      length: 1,
      convRule: rule22
  }, {
      start: 11380,
      length: 1,
      convRule: rule14
  }, {
      start: 11381,
      length: 1,
      convRule: rule21
  }, {
      start: 11382,
      length: 1,
      convRule: rule22
  }, {
      start: 11383,
      length: 6,
      convRule: rule14
  }, {
      start: 11389,
      length: 1,
      convRule: rule83
  }, {
      start: 11390,
      length: 2,
      convRule: rule159
  }, {
      start: 11392,
      length: 1,
      convRule: rule21
  }, {
      start: 11393,
      length: 1,
      convRule: rule22
  }, {
      start: 11394,
      length: 1,
      convRule: rule21
  }, {
      start: 11395,
      length: 1,
      convRule: rule22
  }, {
      start: 11396,
      length: 1,
      convRule: rule21
  }, {
      start: 11397,
      length: 1,
      convRule: rule22
  }, {
      start: 11398,
      length: 1,
      convRule: rule21
  }, {
      start: 11399,
      length: 1,
      convRule: rule22
  }, {
      start: 11400,
      length: 1,
      convRule: rule21
  }, {
      start: 11401,
      length: 1,
      convRule: rule22
  }, {
      start: 11402,
      length: 1,
      convRule: rule21
  }, {
      start: 11403,
      length: 1,
      convRule: rule22
  }, {
      start: 11404,
      length: 1,
      convRule: rule21
  }, {
      start: 11405,
      length: 1,
      convRule: rule22
  }, {
      start: 11406,
      length: 1,
      convRule: rule21
  }, {
      start: 11407,
      length: 1,
      convRule: rule22
  }, {
      start: 11408,
      length: 1,
      convRule: rule21
  }, {
      start: 11409,
      length: 1,
      convRule: rule22
  }, {
      start: 11410,
      length: 1,
      convRule: rule21
  }, {
      start: 11411,
      length: 1,
      convRule: rule22
  }, {
      start: 11412,
      length: 1,
      convRule: rule21
  }, {
      start: 11413,
      length: 1,
      convRule: rule22
  }, {
      start: 11414,
      length: 1,
      convRule: rule21
  }, {
      start: 11415,
      length: 1,
      convRule: rule22
  }, {
      start: 11416,
      length: 1,
      convRule: rule21
  }, {
      start: 11417,
      length: 1,
      convRule: rule22
  }, {
      start: 11418,
      length: 1,
      convRule: rule21
  }, {
      start: 11419,
      length: 1,
      convRule: rule22
  }, {
      start: 11420,
      length: 1,
      convRule: rule21
  }, {
      start: 11421,
      length: 1,
      convRule: rule22
  }, {
      start: 11422,
      length: 1,
      convRule: rule21
  }, {
      start: 11423,
      length: 1,
      convRule: rule22
  }, {
      start: 11424,
      length: 1,
      convRule: rule21
  }, {
      start: 11425,
      length: 1,
      convRule: rule22
  }, {
      start: 11426,
      length: 1,
      convRule: rule21
  }, {
      start: 11427,
      length: 1,
      convRule: rule22
  }, {
      start: 11428,
      length: 1,
      convRule: rule21
  }, {
      start: 11429,
      length: 1,
      convRule: rule22
  }, {
      start: 11430,
      length: 1,
      convRule: rule21
  }, {
      start: 11431,
      length: 1,
      convRule: rule22
  }, {
      start: 11432,
      length: 1,
      convRule: rule21
  }, {
      start: 11433,
      length: 1,
      convRule: rule22
  }, {
      start: 11434,
      length: 1,
      convRule: rule21
  }, {
      start: 11435,
      length: 1,
      convRule: rule22
  }, {
      start: 11436,
      length: 1,
      convRule: rule21
  }, {
      start: 11437,
      length: 1,
      convRule: rule22
  }, {
      start: 11438,
      length: 1,
      convRule: rule21
  }, {
      start: 11439,
      length: 1,
      convRule: rule22
  }, {
      start: 11440,
      length: 1,
      convRule: rule21
  }, {
      start: 11441,
      length: 1,
      convRule: rule22
  }, {
      start: 11442,
      length: 1,
      convRule: rule21
  }, {
      start: 11443,
      length: 1,
      convRule: rule22
  }, {
      start: 11444,
      length: 1,
      convRule: rule21
  }, {
      start: 11445,
      length: 1,
      convRule: rule22
  }, {
      start: 11446,
      length: 1,
      convRule: rule21
  }, {
      start: 11447,
      length: 1,
      convRule: rule22
  }, {
      start: 11448,
      length: 1,
      convRule: rule21
  }, {
      start: 11449,
      length: 1,
      convRule: rule22
  }, {
      start: 11450,
      length: 1,
      convRule: rule21
  }, {
      start: 11451,
      length: 1,
      convRule: rule22
  }, {
      start: 11452,
      length: 1,
      convRule: rule21
  }, {
      start: 11453,
      length: 1,
      convRule: rule22
  }, {
      start: 11454,
      length: 1,
      convRule: rule21
  }, {
      start: 11455,
      length: 1,
      convRule: rule22
  }, {
      start: 11456,
      length: 1,
      convRule: rule21
  }, {
      start: 11457,
      length: 1,
      convRule: rule22
  }, {
      start: 11458,
      length: 1,
      convRule: rule21
  }, {
      start: 11459,
      length: 1,
      convRule: rule22
  }, {
      start: 11460,
      length: 1,
      convRule: rule21
  }, {
      start: 11461,
      length: 1,
      convRule: rule22
  }, {
      start: 11462,
      length: 1,
      convRule: rule21
  }, {
      start: 11463,
      length: 1,
      convRule: rule22
  }, {
      start: 11464,
      length: 1,
      convRule: rule21
  }, {
      start: 11465,
      length: 1,
      convRule: rule22
  }, {
      start: 11466,
      length: 1,
      convRule: rule21
  }, {
      start: 11467,
      length: 1,
      convRule: rule22
  }, {
      start: 11468,
      length: 1,
      convRule: rule21
  }, {
      start: 11469,
      length: 1,
      convRule: rule22
  }, {
      start: 11470,
      length: 1,
      convRule: rule21
  }, {
      start: 11471,
      length: 1,
      convRule: rule22
  }, {
      start: 11472,
      length: 1,
      convRule: rule21
  }, {
      start: 11473,
      length: 1,
      convRule: rule22
  }, {
      start: 11474,
      length: 1,
      convRule: rule21
  }, {
      start: 11475,
      length: 1,
      convRule: rule22
  }, {
      start: 11476,
      length: 1,
      convRule: rule21
  }, {
      start: 11477,
      length: 1,
      convRule: rule22
  }, {
      start: 11478,
      length: 1,
      convRule: rule21
  }, {
      start: 11479,
      length: 1,
      convRule: rule22
  }, {
      start: 11480,
      length: 1,
      convRule: rule21
  }, {
      start: 11481,
      length: 1,
      convRule: rule22
  }, {
      start: 11482,
      length: 1,
      convRule: rule21
  }, {
      start: 11483,
      length: 1,
      convRule: rule22
  }, {
      start: 11484,
      length: 1,
      convRule: rule21
  }, {
      start: 11485,
      length: 1,
      convRule: rule22
  }, {
      start: 11486,
      length: 1,
      convRule: rule21
  }, {
      start: 11487,
      length: 1,
      convRule: rule22
  }, {
      start: 11488,
      length: 1,
      convRule: rule21
  }, {
      start: 11489,
      length: 1,
      convRule: rule22
  }, {
      start: 11490,
      length: 1,
      convRule: rule21
  }, {
      start: 11491,
      length: 1,
      convRule: rule22
  }, {
      start: 11492,
      length: 1,
      convRule: rule14
  }, {
      start: 11493,
      length: 6,
      convRule: rule13
  }, {
      start: 11499,
      length: 1,
      convRule: rule21
  }, {
      start: 11500,
      length: 1,
      convRule: rule22
  }, {
      start: 11501,
      length: 1,
      convRule: rule21
  }, {
      start: 11502,
      length: 1,
      convRule: rule22
  }, {
      start: 11503,
      length: 3,
      convRule: rule84
  }, {
      start: 11513,
      length: 4,
      convRule: rule2
  }, {
      start: 11517,
      length: 1,
      convRule: rule17
  }, {
      start: 11518,
      length: 2,
      convRule: rule2
  }, {
      start: 11520,
      length: 38,
      convRule: rule160
  }, {
      start: 11568,
      length: 54,
      convRule: rule45
  }, {
      start: 11631,
      length: 1,
      convRule: rule83
  }, {
      start: 11632,
      length: 1,
      convRule: rule2
  }, {
      start: 11647,
      length: 1,
      convRule: rule84
  }, {
      start: 11648,
      length: 23,
      convRule: rule45
  }, {
      start: 11680,
      length: 7,
      convRule: rule45
  }, {
      start: 11688,
      length: 7,
      convRule: rule45
  }, {
      start: 11696,
      length: 7,
      convRule: rule45
  }, {
      start: 11704,
      length: 7,
      convRule: rule45
  }, {
      start: 11712,
      length: 7,
      convRule: rule45
  }, {
      start: 11720,
      length: 7,
      convRule: rule45
  }, {
      start: 11728,
      length: 7,
      convRule: rule45
  }, {
      start: 11736,
      length: 7,
      convRule: rule45
  }, {
      start: 11744,
      length: 32,
      convRule: rule84
  }, {
      start: 11776,
      length: 2,
      convRule: rule2
  }, {
      start: 11778,
      length: 1,
      convRule: rule15
  }, {
      start: 11779,
      length: 1,
      convRule: rule19
  }, {
      start: 11780,
      length: 1,
      convRule: rule15
  }, {
      start: 11781,
      length: 1,
      convRule: rule19
  }, {
      start: 11782,
      length: 3,
      convRule: rule2
  }, {
      start: 11785,
      length: 1,
      convRule: rule15
  }, {
      start: 11786,
      length: 1,
      convRule: rule19
  }, {
      start: 11787,
      length: 1,
      convRule: rule2
  }, {
      start: 11788,
      length: 1,
      convRule: rule15
  }, {
      start: 11789,
      length: 1,
      convRule: rule19
  }, {
      start: 11790,
      length: 9,
      convRule: rule2
  }, {
      start: 11799,
      length: 1,
      convRule: rule7
  }, {
      start: 11800,
      length: 2,
      convRule: rule2
  }, {
      start: 11802,
      length: 1,
      convRule: rule7
  }, {
      start: 11803,
      length: 1,
      convRule: rule2
  }, {
      start: 11804,
      length: 1,
      convRule: rule15
  }, {
      start: 11805,
      length: 1,
      convRule: rule19
  }, {
      start: 11806,
      length: 2,
      convRule: rule2
  }, {
      start: 11808,
      length: 1,
      convRule: rule15
  }, {
      start: 11809,
      length: 1,
      convRule: rule19
  }, {
      start: 11810,
      length: 1,
      convRule: rule4
  }, {
      start: 11811,
      length: 1,
      convRule: rule5
  }, {
      start: 11812,
      length: 1,
      convRule: rule4
  }, {
      start: 11813,
      length: 1,
      convRule: rule5
  }, {
      start: 11814,
      length: 1,
      convRule: rule4
  }, {
      start: 11815,
      length: 1,
      convRule: rule5
  }, {
      start: 11816,
      length: 1,
      convRule: rule4
  }, {
      start: 11817,
      length: 1,
      convRule: rule5
  }, {
      start: 11818,
      length: 5,
      convRule: rule2
  }, {
      start: 11823,
      length: 1,
      convRule: rule83
  }, {
      start: 11824,
      length: 2,
      convRule: rule2
  }, {
      start: 11904,
      length: 26,
      convRule: rule13
  }, {
      start: 11931,
      length: 89,
      convRule: rule13
  }, {
      start: 12032,
      length: 214,
      convRule: rule13
  }, {
      start: 12272,
      length: 12,
      convRule: rule13
  }, {
      start: 12288,
      length: 1,
      convRule: rule1
  }, {
      start: 12289,
      length: 3,
      convRule: rule2
  }, {
      start: 12292,
      length: 1,
      convRule: rule13
  }, {
      start: 12293,
      length: 1,
      convRule: rule83
  }, {
      start: 12294,
      length: 1,
      convRule: rule45
  }, {
      start: 12295,
      length: 1,
      convRule: rule116
  }, {
      start: 12296,
      length: 1,
      convRule: rule4
  }, {
      start: 12297,
      length: 1,
      convRule: rule5
  }, {
      start: 12298,
      length: 1,
      convRule: rule4
  }, {
      start: 12299,
      length: 1,
      convRule: rule5
  }, {
      start: 12300,
      length: 1,
      convRule: rule4
  }, {
      start: 12301,
      length: 1,
      convRule: rule5
  }, {
      start: 12302,
      length: 1,
      convRule: rule4
  }, {
      start: 12303,
      length: 1,
      convRule: rule5
  }, {
      start: 12304,
      length: 1,
      convRule: rule4
  }, {
      start: 12305,
      length: 1,
      convRule: rule5
  }, {
      start: 12306,
      length: 2,
      convRule: rule13
  }, {
      start: 12308,
      length: 1,
      convRule: rule4
  }, {
      start: 12309,
      length: 1,
      convRule: rule5
  }, {
      start: 12310,
      length: 1,
      convRule: rule4
  }, {
      start: 12311,
      length: 1,
      convRule: rule5
  }, {
      start: 12312,
      length: 1,
      convRule: rule4
  }, {
      start: 12313,
      length: 1,
      convRule: rule5
  }, {
      start: 12314,
      length: 1,
      convRule: rule4
  }, {
      start: 12315,
      length: 1,
      convRule: rule5
  }, {
      start: 12316,
      length: 1,
      convRule: rule7
  }, {
      start: 12317,
      length: 1,
      convRule: rule4
  }, {
      start: 12318,
      length: 2,
      convRule: rule5
  }, {
      start: 12320,
      length: 1,
      convRule: rule13
  }, {
      start: 12321,
      length: 9,
      convRule: rule116
  }, {
      start: 12330,
      length: 6,
      convRule: rule84
  }, {
      start: 12336,
      length: 1,
      convRule: rule7
  }, {
      start: 12337,
      length: 5,
      convRule: rule83
  }, {
      start: 12342,
      length: 2,
      convRule: rule13
  }, {
      start: 12344,
      length: 3,
      convRule: rule116
  }, {
      start: 12347,
      length: 1,
      convRule: rule83
  }, {
      start: 12348,
      length: 1,
      convRule: rule45
  }, {
      start: 12349,
      length: 1,
      convRule: rule2
  }, {
      start: 12350,
      length: 2,
      convRule: rule13
  }, {
      start: 12353,
      length: 86,
      convRule: rule45
  }, {
      start: 12441,
      length: 2,
      convRule: rule84
  }, {
      start: 12443,
      length: 2,
      convRule: rule10
  }, {
      start: 12445,
      length: 2,
      convRule: rule83
  }, {
      start: 12447,
      length: 1,
      convRule: rule45
  }, {
      start: 12448,
      length: 1,
      convRule: rule7
  }, {
      start: 12449,
      length: 90,
      convRule: rule45
  }, {
      start: 12539,
      length: 1,
      convRule: rule2
  }, {
      start: 12540,
      length: 3,
      convRule: rule83
  }, {
      start: 12543,
      length: 1,
      convRule: rule45
  }, {
      start: 12549,
      length: 41,
      convRule: rule45
  }, {
      start: 12593,
      length: 94,
      convRule: rule45
  }, {
      start: 12688,
      length: 2,
      convRule: rule13
  }, {
      start: 12690,
      length: 4,
      convRule: rule17
  }, {
      start: 12694,
      length: 10,
      convRule: rule13
  }, {
      start: 12704,
      length: 27,
      convRule: rule45
  }, {
      start: 12736,
      length: 36,
      convRule: rule13
  }, {
      start: 12784,
      length: 16,
      convRule: rule45
  }, {
      start: 12800,
      length: 31,
      convRule: rule13
  }, {
      start: 12832,
      length: 10,
      convRule: rule17
  }, {
      start: 12842,
      length: 39,
      convRule: rule13
  }, {
      start: 12881,
      length: 15,
      convRule: rule17
  }, {
      start: 12896,
      length: 32,
      convRule: rule13
  }, {
      start: 12928,
      length: 10,
      convRule: rule17
  }, {
      start: 12938,
      length: 39,
      convRule: rule13
  }, {
      start: 12977,
      length: 15,
      convRule: rule17
  }, {
      start: 12992,
      length: 63,
      convRule: rule13
  }, {
      start: 13056,
      length: 256,
      convRule: rule13
  }, {
      start: 13312,
      length: 6582,
      convRule: rule45
  }, {
      start: 19904,
      length: 64,
      convRule: rule13
  }, {
      start: 19968,
      length: 20940,
      convRule: rule45
  }, {
      start: 40960,
      length: 21,
      convRule: rule45
  }, {
      start: 40981,
      length: 1,
      convRule: rule83
  }, {
      start: 40982,
      length: 1143,
      convRule: rule45
  }, {
      start: 42128,
      length: 55,
      convRule: rule13
  }, {
      start: 42192,
      length: 40,
      convRule: rule45
  }, {
      start: 42232,
      length: 6,
      convRule: rule83
  }, {
      start: 42238,
      length: 2,
      convRule: rule2
  }, {
      start: 42240,
      length: 268,
      convRule: rule45
  }, {
      start: 42508,
      length: 1,
      convRule: rule83
  }, {
      start: 42509,
      length: 3,
      convRule: rule2
  }, {
      start: 42512,
      length: 16,
      convRule: rule45
  }, {
      start: 42528,
      length: 10,
      convRule: rule8
  }, {
      start: 42538,
      length: 2,
      convRule: rule45
  }, {
      start: 42560,
      length: 1,
      convRule: rule21
  }, {
      start: 42561,
      length: 1,
      convRule: rule22
  }, {
      start: 42562,
      length: 1,
      convRule: rule21
  }, {
      start: 42563,
      length: 1,
      convRule: rule22
  }, {
      start: 42564,
      length: 1,
      convRule: rule21
  }, {
      start: 42565,
      length: 1,
      convRule: rule22
  }, {
      start: 42566,
      length: 1,
      convRule: rule21
  }, {
      start: 42567,
      length: 1,
      convRule: rule22
  }, {
      start: 42568,
      length: 1,
      convRule: rule21
  }, {
      start: 42569,
      length: 1,
      convRule: rule22
  }, {
      start: 42570,
      length: 1,
      convRule: rule21
  }, {
      start: 42571,
      length: 1,
      convRule: rule22
  }, {
      start: 42572,
      length: 1,
      convRule: rule21
  }, {
      start: 42573,
      length: 1,
      convRule: rule22
  }, {
      start: 42574,
      length: 1,
      convRule: rule21
  }, {
      start: 42575,
      length: 1,
      convRule: rule22
  }, {
      start: 42576,
      length: 1,
      convRule: rule21
  }, {
      start: 42577,
      length: 1,
      convRule: rule22
  }, {
      start: 42578,
      length: 1,
      convRule: rule21
  }, {
      start: 42579,
      length: 1,
      convRule: rule22
  }, {
      start: 42580,
      length: 1,
      convRule: rule21
  }, {
      start: 42581,
      length: 1,
      convRule: rule22
  }, {
      start: 42582,
      length: 1,
      convRule: rule21
  }, {
      start: 42583,
      length: 1,
      convRule: rule22
  }, {
      start: 42584,
      length: 1,
      convRule: rule21
  }, {
      start: 42585,
      length: 1,
      convRule: rule22
  }, {
      start: 42586,
      length: 1,
      convRule: rule21
  }, {
      start: 42587,
      length: 1,
      convRule: rule22
  }, {
      start: 42588,
      length: 1,
      convRule: rule21
  }, {
      start: 42589,
      length: 1,
      convRule: rule22
  }, {
      start: 42590,
      length: 1,
      convRule: rule21
  }, {
      start: 42591,
      length: 1,
      convRule: rule22
  }, {
      start: 42592,
      length: 1,
      convRule: rule21
  }, {
      start: 42593,
      length: 1,
      convRule: rule22
  }, {
      start: 42594,
      length: 1,
      convRule: rule21
  }, {
      start: 42595,
      length: 1,
      convRule: rule22
  }, {
      start: 42596,
      length: 1,
      convRule: rule21
  }, {
      start: 42597,
      length: 1,
      convRule: rule22
  }, {
      start: 42598,
      length: 1,
      convRule: rule21
  }, {
      start: 42599,
      length: 1,
      convRule: rule22
  }, {
      start: 42600,
      length: 1,
      convRule: rule21
  }, {
      start: 42601,
      length: 1,
      convRule: rule22
  }, {
      start: 42602,
      length: 1,
      convRule: rule21
  }, {
      start: 42603,
      length: 1,
      convRule: rule22
  }, {
      start: 42604,
      length: 1,
      convRule: rule21
  }, {
      start: 42605,
      length: 1,
      convRule: rule22
  }, {
      start: 42606,
      length: 1,
      convRule: rule45
  }, {
      start: 42607,
      length: 1,
      convRule: rule84
  }, {
      start: 42608,
      length: 3,
      convRule: rule109
  }, {
      start: 42611,
      length: 1,
      convRule: rule2
  }, {
      start: 42620,
      length: 2,
      convRule: rule84
  }, {
      start: 42622,
      length: 1,
      convRule: rule2
  }, {
      start: 42623,
      length: 1,
      convRule: rule83
  }, {
      start: 42624,
      length: 1,
      convRule: rule21
  }, {
      start: 42625,
      length: 1,
      convRule: rule22
  }, {
      start: 42626,
      length: 1,
      convRule: rule21
  }, {
      start: 42627,
      length: 1,
      convRule: rule22
  }, {
      start: 42628,
      length: 1,
      convRule: rule21
  }, {
      start: 42629,
      length: 1,
      convRule: rule22
  }, {
      start: 42630,
      length: 1,
      convRule: rule21
  }, {
      start: 42631,
      length: 1,
      convRule: rule22
  }, {
      start: 42632,
      length: 1,
      convRule: rule21
  }, {
      start: 42633,
      length: 1,
      convRule: rule22
  }, {
      start: 42634,
      length: 1,
      convRule: rule21
  }, {
      start: 42635,
      length: 1,
      convRule: rule22
  }, {
      start: 42636,
      length: 1,
      convRule: rule21
  }, {
      start: 42637,
      length: 1,
      convRule: rule22
  }, {
      start: 42638,
      length: 1,
      convRule: rule21
  }, {
      start: 42639,
      length: 1,
      convRule: rule22
  }, {
      start: 42640,
      length: 1,
      convRule: rule21
  }, {
      start: 42641,
      length: 1,
      convRule: rule22
  }, {
      start: 42642,
      length: 1,
      convRule: rule21
  }, {
      start: 42643,
      length: 1,
      convRule: rule22
  }, {
      start: 42644,
      length: 1,
      convRule: rule21
  }, {
      start: 42645,
      length: 1,
      convRule: rule22
  }, {
      start: 42646,
      length: 1,
      convRule: rule21
  }, {
      start: 42647,
      length: 1,
      convRule: rule22
  }, {
      start: 42656,
      length: 70,
      convRule: rule45
  }, {
      start: 42726,
      length: 10,
      convRule: rule116
  }, {
      start: 42736,
      length: 2,
      convRule: rule84
  }, {
      start: 42738,
      length: 6,
      convRule: rule2
  }, {
      start: 42752,
      length: 23,
      convRule: rule10
  }, {
      start: 42775,
      length: 9,
      convRule: rule83
  }, {
      start: 42784,
      length: 2,
      convRule: rule10
  }, {
      start: 42786,
      length: 1,
      convRule: rule21
  }, {
      start: 42787,
      length: 1,
      convRule: rule22
  }, {
      start: 42788,
      length: 1,
      convRule: rule21
  }, {
      start: 42789,
      length: 1,
      convRule: rule22
  }, {
      start: 42790,
      length: 1,
      convRule: rule21
  }, {
      start: 42791,
      length: 1,
      convRule: rule22
  }, {
      start: 42792,
      length: 1,
      convRule: rule21
  }, {
      start: 42793,
      length: 1,
      convRule: rule22
  }, {
      start: 42794,
      length: 1,
      convRule: rule21
  }, {
      start: 42795,
      length: 1,
      convRule: rule22
  }, {
      start: 42796,
      length: 1,
      convRule: rule21
  }, {
      start: 42797,
      length: 1,
      convRule: rule22
  }, {
      start: 42798,
      length: 1,
      convRule: rule21
  }, {
      start: 42799,
      length: 1,
      convRule: rule22
  }, {
      start: 42800,
      length: 2,
      convRule: rule14
  }, {
      start: 42802,
      length: 1,
      convRule: rule21
  }, {
      start: 42803,
      length: 1,
      convRule: rule22
  }, {
      start: 42804,
      length: 1,
      convRule: rule21
  }, {
      start: 42805,
      length: 1,
      convRule: rule22
  }, {
      start: 42806,
      length: 1,
      convRule: rule21
  }, {
      start: 42807,
      length: 1,
      convRule: rule22
  }, {
      start: 42808,
      length: 1,
      convRule: rule21
  }, {
      start: 42809,
      length: 1,
      convRule: rule22
  }, {
      start: 42810,
      length: 1,
      convRule: rule21
  }, {
      start: 42811,
      length: 1,
      convRule: rule22
  }, {
      start: 42812,
      length: 1,
      convRule: rule21
  }, {
      start: 42813,
      length: 1,
      convRule: rule22
  }, {
      start: 42814,
      length: 1,
      convRule: rule21
  }, {
      start: 42815,
      length: 1,
      convRule: rule22
  }, {
      start: 42816,
      length: 1,
      convRule: rule21
  }, {
      start: 42817,
      length: 1,
      convRule: rule22
  }, {
      start: 42818,
      length: 1,
      convRule: rule21
  }, {
      start: 42819,
      length: 1,
      convRule: rule22
  }, {
      start: 42820,
      length: 1,
      convRule: rule21
  }, {
      start: 42821,
      length: 1,
      convRule: rule22
  }, {
      start: 42822,
      length: 1,
      convRule: rule21
  }, {
      start: 42823,
      length: 1,
      convRule: rule22
  }, {
      start: 42824,
      length: 1,
      convRule: rule21
  }, {
      start: 42825,
      length: 1,
      convRule: rule22
  }, {
      start: 42826,
      length: 1,
      convRule: rule21
  }, {
      start: 42827,
      length: 1,
      convRule: rule22
  }, {
      start: 42828,
      length: 1,
      convRule: rule21
  }, {
      start: 42829,
      length: 1,
      convRule: rule22
  }, {
      start: 42830,
      length: 1,
      convRule: rule21
  }, {
      start: 42831,
      length: 1,
      convRule: rule22
  }, {
      start: 42832,
      length: 1,
      convRule: rule21
  }, {
      start: 42833,
      length: 1,
      convRule: rule22
  }, {
      start: 42834,
      length: 1,
      convRule: rule21
  }, {
      start: 42835,
      length: 1,
      convRule: rule22
  }, {
      start: 42836,
      length: 1,
      convRule: rule21
  }, {
      start: 42837,
      length: 1,
      convRule: rule22
  }, {
      start: 42838,
      length: 1,
      convRule: rule21
  }, {
      start: 42839,
      length: 1,
      convRule: rule22
  }, {
      start: 42840,
      length: 1,
      convRule: rule21
  }, {
      start: 42841,
      length: 1,
      convRule: rule22
  }, {
      start: 42842,
      length: 1,
      convRule: rule21
  }, {
      start: 42843,
      length: 1,
      convRule: rule22
  }, {
      start: 42844,
      length: 1,
      convRule: rule21
  }, {
      start: 42845,
      length: 1,
      convRule: rule22
  }, {
      start: 42846,
      length: 1,
      convRule: rule21
  }, {
      start: 42847,
      length: 1,
      convRule: rule22
  }, {
      start: 42848,
      length: 1,
      convRule: rule21
  }, {
      start: 42849,
      length: 1,
      convRule: rule22
  }, {
      start: 42850,
      length: 1,
      convRule: rule21
  }, {
      start: 42851,
      length: 1,
      convRule: rule22
  }, {
      start: 42852,
      length: 1,
      convRule: rule21
  }, {
      start: 42853,
      length: 1,
      convRule: rule22
  }, {
      start: 42854,
      length: 1,
      convRule: rule21
  }, {
      start: 42855,
      length: 1,
      convRule: rule22
  }, {
      start: 42856,
      length: 1,
      convRule: rule21
  }, {
      start: 42857,
      length: 1,
      convRule: rule22
  }, {
      start: 42858,
      length: 1,
      convRule: rule21
  }, {
      start: 42859,
      length: 1,
      convRule: rule22
  }, {
      start: 42860,
      length: 1,
      convRule: rule21
  }, {
      start: 42861,
      length: 1,
      convRule: rule22
  }, {
      start: 42862,
      length: 1,
      convRule: rule21
  }, {
      start: 42863,
      length: 1,
      convRule: rule22
  }, {
      start: 42864,
      length: 1,
      convRule: rule83
  }, {
      start: 42865,
      length: 8,
      convRule: rule14
  }, {
      start: 42873,
      length: 1,
      convRule: rule21
  }, {
      start: 42874,
      length: 1,
      convRule: rule22
  }, {
      start: 42875,
      length: 1,
      convRule: rule21
  }, {
      start: 42876,
      length: 1,
      convRule: rule22
  }, {
      start: 42877,
      length: 1,
      convRule: rule161
  }, {
      start: 42878,
      length: 1,
      convRule: rule21
  }, {
      start: 42879,
      length: 1,
      convRule: rule22
  }, {
      start: 42880,
      length: 1,
      convRule: rule21
  }, {
      start: 42881,
      length: 1,
      convRule: rule22
  }, {
      start: 42882,
      length: 1,
      convRule: rule21
  }, {
      start: 42883,
      length: 1,
      convRule: rule22
  }, {
      start: 42884,
      length: 1,
      convRule: rule21
  }, {
      start: 42885,
      length: 1,
      convRule: rule22
  }, {
      start: 42886,
      length: 1,
      convRule: rule21
  }, {
      start: 42887,
      length: 1,
      convRule: rule22
  }, {
      start: 42888,
      length: 1,
      convRule: rule83
  }, {
      start: 42889,
      length: 2,
      convRule: rule10
  }, {
      start: 42891,
      length: 1,
      convRule: rule21
  }, {
      start: 42892,
      length: 1,
      convRule: rule22
  }, {
      start: 42893,
      length: 1,
      convRule: rule162
  }, {
      start: 42894,
      length: 1,
      convRule: rule14
  }, {
      start: 42896,
      length: 1,
      convRule: rule21
  }, {
      start: 42897,
      length: 1,
      convRule: rule22
  }, {
      start: 42912,
      length: 1,
      convRule: rule21
  }, {
      start: 42913,
      length: 1,
      convRule: rule22
  }, {
      start: 42914,
      length: 1,
      convRule: rule21
  }, {
      start: 42915,
      length: 1,
      convRule: rule22
  }, {
      start: 42916,
      length: 1,
      convRule: rule21
  }, {
      start: 42917,
      length: 1,
      convRule: rule22
  }, {
      start: 42918,
      length: 1,
      convRule: rule21
  }, {
      start: 42919,
      length: 1,
      convRule: rule22
  }, {
      start: 42920,
      length: 1,
      convRule: rule21
  }, {
      start: 42921,
      length: 1,
      convRule: rule22
  }, {
      start: 43002,
      length: 1,
      convRule: rule14
  }, {
      start: 43003,
      length: 7,
      convRule: rule45
  }, {
      start: 43010,
      length: 1,
      convRule: rule84
  }, {
      start: 43011,
      length: 3,
      convRule: rule45
  }, {
      start: 43014,
      length: 1,
      convRule: rule84
  }, {
      start: 43015,
      length: 4,
      convRule: rule45
  }, {
      start: 43019,
      length: 1,
      convRule: rule84
  }, {
      start: 43020,
      length: 23,
      convRule: rule45
  }, {
      start: 43043,
      length: 2,
      convRule: rule114
  }, {
      start: 43045,
      length: 2,
      convRule: rule84
  }, {
      start: 43047,
      length: 1,
      convRule: rule114
  }, {
      start: 43048,
      length: 4,
      convRule: rule13
  }, {
      start: 43056,
      length: 6,
      convRule: rule17
  }, {
      start: 43062,
      length: 2,
      convRule: rule13
  }, {
      start: 43064,
      length: 1,
      convRule: rule3
  }, {
      start: 43065,
      length: 1,
      convRule: rule13
  }, {
      start: 43072,
      length: 52,
      convRule: rule45
  }, {
      start: 43124,
      length: 4,
      convRule: rule2
  }, {
      start: 43136,
      length: 2,
      convRule: rule114
  }, {
      start: 43138,
      length: 50,
      convRule: rule45
  }, {
      start: 43188,
      length: 16,
      convRule: rule114
  }, {
      start: 43204,
      length: 1,
      convRule: rule84
  }, {
      start: 43214,
      length: 2,
      convRule: rule2
  }, {
      start: 43216,
      length: 10,
      convRule: rule8
  }, {
      start: 43232,
      length: 18,
      convRule: rule84
  }, {
      start: 43250,
      length: 6,
      convRule: rule45
  }, {
      start: 43256,
      length: 3,
      convRule: rule2
  }, {
      start: 43259,
      length: 1,
      convRule: rule45
  }, {
      start: 43264,
      length: 10,
      convRule: rule8
  }, {
      start: 43274,
      length: 28,
      convRule: rule45
  }, {
      start: 43302,
      length: 8,
      convRule: rule84
  }, {
      start: 43310,
      length: 2,
      convRule: rule2
  }, {
      start: 43312,
      length: 23,
      convRule: rule45
  }, {
      start: 43335,
      length: 11,
      convRule: rule84
  }, {
      start: 43346,
      length: 2,
      convRule: rule114
  }, {
      start: 43359,
      length: 1,
      convRule: rule2
  }, {
      start: 43360,
      length: 29,
      convRule: rule45
  }, {
      start: 43392,
      length: 3,
      convRule: rule84
  }, {
      start: 43395,
      length: 1,
      convRule: rule114
  }, {
      start: 43396,
      length: 47,
      convRule: rule45
  }, {
      start: 43443,
      length: 1,
      convRule: rule84
  }, {
      start: 43444,
      length: 2,
      convRule: rule114
  }, {
      start: 43446,
      length: 4,
      convRule: rule84
  }, {
      start: 43450,
      length: 2,
      convRule: rule114
  }, {
      start: 43452,
      length: 1,
      convRule: rule84
  }, {
      start: 43453,
      length: 4,
      convRule: rule114
  }, {
      start: 43457,
      length: 13,
      convRule: rule2
  }, {
      start: 43471,
      length: 1,
      convRule: rule83
  }, {
      start: 43472,
      length: 10,
      convRule: rule8
  }, {
      start: 43486,
      length: 2,
      convRule: rule2
  }, {
      start: 43520,
      length: 41,
      convRule: rule45
  }, {
      start: 43561,
      length: 6,
      convRule: rule84
  }, {
      start: 43567,
      length: 2,
      convRule: rule114
  }, {
      start: 43569,
      length: 2,
      convRule: rule84
  }, {
      start: 43571,
      length: 2,
      convRule: rule114
  }, {
      start: 43573,
      length: 2,
      convRule: rule84
  }, {
      start: 43584,
      length: 3,
      convRule: rule45
  }, {
      start: 43587,
      length: 1,
      convRule: rule84
  }, {
      start: 43588,
      length: 8,
      convRule: rule45
  }, {
      start: 43596,
      length: 1,
      convRule: rule84
  }, {
      start: 43597,
      length: 1,
      convRule: rule114
  }, {
      start: 43600,
      length: 10,
      convRule: rule8
  }, {
      start: 43612,
      length: 4,
      convRule: rule2
  }, {
      start: 43616,
      length: 16,
      convRule: rule45
  }, {
      start: 43632,
      length: 1,
      convRule: rule83
  }, {
      start: 43633,
      length: 6,
      convRule: rule45
  }, {
      start: 43639,
      length: 3,
      convRule: rule13
  }, {
      start: 43642,
      length: 1,
      convRule: rule45
  }, {
      start: 43643,
      length: 1,
      convRule: rule114
  }, {
      start: 43648,
      length: 48,
      convRule: rule45
  }, {
      start: 43696,
      length: 1,
      convRule: rule84
  }, {
      start: 43697,
      length: 1,
      convRule: rule45
  }, {
      start: 43698,
      length: 3,
      convRule: rule84
  }, {
      start: 43701,
      length: 2,
      convRule: rule45
  }, {
      start: 43703,
      length: 2,
      convRule: rule84
  }, {
      start: 43705,
      length: 5,
      convRule: rule45
  }, {
      start: 43710,
      length: 2,
      convRule: rule84
  }, {
      start: 43712,
      length: 1,
      convRule: rule45
  }, {
      start: 43713,
      length: 1,
      convRule: rule84
  }, {
      start: 43714,
      length: 1,
      convRule: rule45
  }, {
      start: 43739,
      length: 2,
      convRule: rule45
  }, {
      start: 43741,
      length: 1,
      convRule: rule83
  }, {
      start: 43742,
      length: 2,
      convRule: rule2
  }, {
      start: 43777,
      length: 6,
      convRule: rule45
  }, {
      start: 43785,
      length: 6,
      convRule: rule45
  }, {
      start: 43793,
      length: 6,
      convRule: rule45
  }, {
      start: 43808,
      length: 7,
      convRule: rule45
  }, {
      start: 43816,
      length: 7,
      convRule: rule45
  }, {
      start: 43968,
      length: 35,
      convRule: rule45
  }, {
      start: 44003,
      length: 2,
      convRule: rule114
  }, {
      start: 44005,
      length: 1,
      convRule: rule84
  }, {
      start: 44006,
      length: 2,
      convRule: rule114
  }, {
      start: 44008,
      length: 1,
      convRule: rule84
  }, {
      start: 44009,
      length: 2,
      convRule: rule114
  }, {
      start: 44011,
      length: 1,
      convRule: rule2
  }, {
      start: 44012,
      length: 1,
      convRule: rule114
  }, {
      start: 44013,
      length: 1,
      convRule: rule84
  }, {
      start: 44016,
      length: 10,
      convRule: rule8
  }, {
      start: 44032,
      length: 11172,
      convRule: rule45
  }, {
      start: 55216,
      length: 23,
      convRule: rule45
  }, {
      start: 55243,
      length: 49,
      convRule: rule45
  }, {
      start: 55296,
      length: 896,
      convRule: rule163
  }, {
      start: 56192,
      length: 128,
      convRule: rule163
  }, {
      start: 56320,
      length: 1024,
      convRule: rule163
  }, {
      start: 57344,
      length: 6400,
      convRule: rule164
  }, {
      start: 63744,
      length: 302,
      convRule: rule45
  }, {
      start: 64048,
      length: 62,
      convRule: rule45
  }, {
      start: 64112,
      length: 106,
      convRule: rule45
  }, {
      start: 64256,
      length: 7,
      convRule: rule14
  }, {
      start: 64275,
      length: 5,
      convRule: rule14
  }, {
      start: 64285,
      length: 1,
      convRule: rule45
  }, {
      start: 64286,
      length: 1,
      convRule: rule84
  }, {
      start: 64287,
      length: 10,
      convRule: rule45
  }, {
      start: 64297,
      length: 1,
      convRule: rule6
  }, {
      start: 64298,
      length: 13,
      convRule: rule45
  }, {
      start: 64312,
      length: 5,
      convRule: rule45
  }, {
      start: 64318,
      length: 1,
      convRule: rule45
  }, {
      start: 64320,
      length: 2,
      convRule: rule45
  }, {
      start: 64323,
      length: 2,
      convRule: rule45
  }, {
      start: 64326,
      length: 108,
      convRule: rule45
  }, {
      start: 64434,
      length: 16,
      convRule: rule10
  }, {
      start: 64467,
      length: 363,
      convRule: rule45
  }, {
      start: 64830,
      length: 1,
      convRule: rule4
  }, {
      start: 64831,
      length: 1,
      convRule: rule5
  }, {
      start: 64848,
      length: 64,
      convRule: rule45
  }, {
      start: 64914,
      length: 54,
      convRule: rule45
  }, {
      start: 65008,
      length: 12,
      convRule: rule45
  }, {
      start: 65020,
      length: 1,
      convRule: rule3
  }, {
      start: 65021,
      length: 1,
      convRule: rule13
  }, {
      start: 65024,
      length: 16,
      convRule: rule84
  }, {
      start: 65040,
      length: 7,
      convRule: rule2
  }, {
      start: 65047,
      length: 1,
      convRule: rule4
  }, {
      start: 65048,
      length: 1,
      convRule: rule5
  }, {
      start: 65049,
      length: 1,
      convRule: rule2
  }, {
      start: 65056,
      length: 7,
      convRule: rule84
  }, {
      start: 65072,
      length: 1,
      convRule: rule2
  }, {
      start: 65073,
      length: 2,
      convRule: rule7
  }, {
      start: 65075,
      length: 2,
      convRule: rule11
  }, {
      start: 65077,
      length: 1,
      convRule: rule4
  }, {
      start: 65078,
      length: 1,
      convRule: rule5
  }, {
      start: 65079,
      length: 1,
      convRule: rule4
  }, {
      start: 65080,
      length: 1,
      convRule: rule5
  }, {
      start: 65081,
      length: 1,
      convRule: rule4
  }, {
      start: 65082,
      length: 1,
      convRule: rule5
  }, {
      start: 65083,
      length: 1,
      convRule: rule4
  }, {
      start: 65084,
      length: 1,
      convRule: rule5
  }, {
      start: 65085,
      length: 1,
      convRule: rule4
  }, {
      start: 65086,
      length: 1,
      convRule: rule5
  }, {
      start: 65087,
      length: 1,
      convRule: rule4
  }, {
      start: 65088,
      length: 1,
      convRule: rule5
  }, {
      start: 65089,
      length: 1,
      convRule: rule4
  }, {
      start: 65090,
      length: 1,
      convRule: rule5
  }, {
      start: 65091,
      length: 1,
      convRule: rule4
  }, {
      start: 65092,
      length: 1,
      convRule: rule5
  }, {
      start: 65093,
      length: 2,
      convRule: rule2
  }, {
      start: 65095,
      length: 1,
      convRule: rule4
  }, {
      start: 65096,
      length: 1,
      convRule: rule5
  }, {
      start: 65097,
      length: 4,
      convRule: rule2
  }, {
      start: 65101,
      length: 3,
      convRule: rule11
  }, {
      start: 65104,
      length: 3,
      convRule: rule2
  }, {
      start: 65108,
      length: 4,
      convRule: rule2
  }, {
      start: 65112,
      length: 1,
      convRule: rule7
  }, {
      start: 65113,
      length: 1,
      convRule: rule4
  }, {
      start: 65114,
      length: 1,
      convRule: rule5
  }, {
      start: 65115,
      length: 1,
      convRule: rule4
  }, {
      start: 65116,
      length: 1,
      convRule: rule5
  }, {
      start: 65117,
      length: 1,
      convRule: rule4
  }, {
      start: 65118,
      length: 1,
      convRule: rule5
  }, {
      start: 65119,
      length: 3,
      convRule: rule2
  }, {
      start: 65122,
      length: 1,
      convRule: rule6
  }, {
      start: 65123,
      length: 1,
      convRule: rule7
  }, {
      start: 65124,
      length: 3,
      convRule: rule6
  }, {
      start: 65128,
      length: 1,
      convRule: rule2
  }, {
      start: 65129,
      length: 1,
      convRule: rule3
  }, {
      start: 65130,
      length: 2,
      convRule: rule2
  }, {
      start: 65136,
      length: 5,
      convRule: rule45
  }, {
      start: 65142,
      length: 135,
      convRule: rule45
  }, {
      start: 65279,
      length: 1,
      convRule: rule16
  }, {
      start: 65281,
      length: 3,
      convRule: rule2
  }, {
      start: 65284,
      length: 1,
      convRule: rule3
  }, {
      start: 65285,
      length: 3,
      convRule: rule2
  }, {
      start: 65288,
      length: 1,
      convRule: rule4
  }, {
      start: 65289,
      length: 1,
      convRule: rule5
  }, {
      start: 65290,
      length: 1,
      convRule: rule2
  }, {
      start: 65291,
      length: 1,
      convRule: rule6
  }, {
      start: 65292,
      length: 1,
      convRule: rule2
  }, {
      start: 65293,
      length: 1,
      convRule: rule7
  }, {
      start: 65294,
      length: 2,
      convRule: rule2
  }, {
      start: 65296,
      length: 10,
      convRule: rule8
  }, {
      start: 65306,
      length: 2,
      convRule: rule2
  }, {
      start: 65308,
      length: 3,
      convRule: rule6
  }, {
      start: 65311,
      length: 2,
      convRule: rule2
  }, {
      start: 65313,
      length: 26,
      convRule: rule9
  }, {
      start: 65339,
      length: 1,
      convRule: rule4
  }, {
      start: 65340,
      length: 1,
      convRule: rule2
  }, {
      start: 65341,
      length: 1,
      convRule: rule5
  }, {
      start: 65342,
      length: 1,
      convRule: rule10
  }, {
      start: 65343,
      length: 1,
      convRule: rule11
  }, {
      start: 65344,
      length: 1,
      convRule: rule10
  }, {
      start: 65345,
      length: 26,
      convRule: rule12
  }, {
      start: 65371,
      length: 1,
      convRule: rule4
  }, {
      start: 65372,
      length: 1,
      convRule: rule6
  }, {
      start: 65373,
      length: 1,
      convRule: rule5
  }, {
      start: 65374,
      length: 1,
      convRule: rule6
  }, {
      start: 65375,
      length: 1,
      convRule: rule4
  }, {
      start: 65376,
      length: 1,
      convRule: rule5
  }, {
      start: 65377,
      length: 1,
      convRule: rule2
  }, {
      start: 65378,
      length: 1,
      convRule: rule4
  }, {
      start: 65379,
      length: 1,
      convRule: rule5
  }, {
      start: 65380,
      length: 2,
      convRule: rule2
  }, {
      start: 65382,
      length: 10,
      convRule: rule45
  }, {
      start: 65392,
      length: 1,
      convRule: rule83
  }, {
      start: 65393,
      length: 45,
      convRule: rule45
  }, {
      start: 65438,
      length: 2,
      convRule: rule83
  }, {
      start: 65440,
      length: 31,
      convRule: rule45
  }, {
      start: 65474,
      length: 6,
      convRule: rule45
  }, {
      start: 65482,
      length: 6,
      convRule: rule45
  }, {
      start: 65490,
      length: 6,
      convRule: rule45
  }, {
      start: 65498,
      length: 3,
      convRule: rule45
  }, {
      start: 65504,
      length: 2,
      convRule: rule3
  }, {
      start: 65506,
      length: 1,
      convRule: rule6
  }, {
      start: 65507,
      length: 1,
      convRule: rule10
  }, {
      start: 65508,
      length: 1,
      convRule: rule13
  }, {
      start: 65509,
      length: 2,
      convRule: rule3
  }, {
      start: 65512,
      length: 1,
      convRule: rule13
  }, {
      start: 65513,
      length: 4,
      convRule: rule6
  }, {
      start: 65517,
      length: 2,
      convRule: rule13
  }, {
      start: 65529,
      length: 3,
      convRule: rule16
  }, {
      start: 65532,
      length: 2,
      convRule: rule13
  }, {
      start: 65536,
      length: 12,
      convRule: rule45
  }, {
      start: 65549,
      length: 26,
      convRule: rule45
  }, {
      start: 65576,
      length: 19,
      convRule: rule45
  }, {
      start: 65596,
      length: 2,
      convRule: rule45
  }, {
      start: 65599,
      length: 15,
      convRule: rule45
  }, {
      start: 65616,
      length: 14,
      convRule: rule45
  }, {
      start: 65664,
      length: 123,
      convRule: rule45
  }, {
      start: 65792,
      length: 2,
      convRule: rule2
  }, {
      start: 65794,
      length: 1,
      convRule: rule13
  }, {
      start: 65799,
      length: 45,
      convRule: rule17
  }, {
      start: 65847,
      length: 9,
      convRule: rule13
  }, {
      start: 65856,
      length: 53,
      convRule: rule116
  }, {
      start: 65909,
      length: 4,
      convRule: rule17
  }, {
      start: 65913,
      length: 17,
      convRule: rule13
  }, {
      start: 65930,
      length: 1,
      convRule: rule17
  }, {
      start: 65936,
      length: 12,
      convRule: rule13
  }, {
      start: 66000,
      length: 45,
      convRule: rule13
  }, {
      start: 66045,
      length: 1,
      convRule: rule84
  }, {
      start: 66176,
      length: 29,
      convRule: rule45
  }, {
      start: 66208,
      length: 49,
      convRule: rule45
  }, {
      start: 66304,
      length: 31,
      convRule: rule45
  }, {
      start: 66336,
      length: 4,
      convRule: rule17
  }, {
      start: 66352,
      length: 17,
      convRule: rule45
  }, {
      start: 66369,
      length: 1,
      convRule: rule116
  }, {
      start: 66370,
      length: 8,
      convRule: rule45
  }, {
      start: 66378,
      length: 1,
      convRule: rule116
  }, {
      start: 66432,
      length: 30,
      convRule: rule45
  }, {
      start: 66463,
      length: 1,
      convRule: rule2
  }, {
      start: 66464,
      length: 36,
      convRule: rule45
  }, {
      start: 66504,
      length: 8,
      convRule: rule45
  }, {
      start: 66512,
      length: 1,
      convRule: rule2
  }, {
      start: 66513,
      length: 5,
      convRule: rule116
  }, {
      start: 66560,
      length: 40,
      convRule: rule165
  }, {
      start: 66600,
      length: 40,
      convRule: rule166
  }, {
      start: 66640,
      length: 78,
      convRule: rule45
  }, {
      start: 66720,
      length: 10,
      convRule: rule8
  }, {
      start: 67584,
      length: 6,
      convRule: rule45
  }, {
      start: 67592,
      length: 1,
      convRule: rule45
  }, {
      start: 67594,
      length: 44,
      convRule: rule45
  }, {
      start: 67639,
      length: 2,
      convRule: rule45
  }, {
      start: 67644,
      length: 1,
      convRule: rule45
  }, {
      start: 67647,
      length: 23,
      convRule: rule45
  }, {
      start: 67671,
      length: 1,
      convRule: rule2
  }, {
      start: 67672,
      length: 8,
      convRule: rule17
  }, {
      start: 67840,
      length: 22,
      convRule: rule45
  }, {
      start: 67862,
      length: 6,
      convRule: rule17
  }, {
      start: 67871,
      length: 1,
      convRule: rule2
  }, {
      start: 67872,
      length: 26,
      convRule: rule45
  }, {
      start: 67903,
      length: 1,
      convRule: rule2
  }, {
      start: 68096,
      length: 1,
      convRule: rule45
  }, {
      start: 68097,
      length: 3,
      convRule: rule84
  }, {
      start: 68101,
      length: 2,
      convRule: rule84
  }, {
      start: 68108,
      length: 4,
      convRule: rule84
  }, {
      start: 68112,
      length: 4,
      convRule: rule45
  }, {
      start: 68117,
      length: 3,
      convRule: rule45
  }, {
      start: 68121,
      length: 27,
      convRule: rule45
  }, {
      start: 68152,
      length: 3,
      convRule: rule84
  }, {
      start: 68159,
      length: 1,
      convRule: rule84
  }, {
      start: 68160,
      length: 8,
      convRule: rule17
  }, {
      start: 68176,
      length: 9,
      convRule: rule2
  }, {
      start: 68192,
      length: 29,
      convRule: rule45
  }, {
      start: 68221,
      length: 2,
      convRule: rule17
  }, {
      start: 68223,
      length: 1,
      convRule: rule2
  }, {
      start: 68352,
      length: 54,
      convRule: rule45
  }, {
      start: 68409,
      length: 7,
      convRule: rule2
  }, {
      start: 68416,
      length: 22,
      convRule: rule45
  }, {
      start: 68440,
      length: 8,
      convRule: rule17
  }, {
      start: 68448,
      length: 19,
      convRule: rule45
  }, {
      start: 68472,
      length: 8,
      convRule: rule17
  }, {
      start: 68608,
      length: 73,
      convRule: rule45
  }, {
      start: 69216,
      length: 31,
      convRule: rule17
  }, {
      start: 69632,
      length: 1,
      convRule: rule114
  }, {
      start: 69633,
      length: 1,
      convRule: rule84
  }, {
      start: 69634,
      length: 1,
      convRule: rule114
  }, {
      start: 69635,
      length: 53,
      convRule: rule45
  }, {
      start: 69688,
      length: 15,
      convRule: rule84
  }, {
      start: 69703,
      length: 7,
      convRule: rule2
  }, {
      start: 69714,
      length: 20,
      convRule: rule17
  }, {
      start: 69734,
      length: 10,
      convRule: rule8
  }, {
      start: 69760,
      length: 2,
      convRule: rule84
  }, {
      start: 69762,
      length: 1,
      convRule: rule114
  }, {
      start: 69763,
      length: 45,
      convRule: rule45
  }, {
      start: 69808,
      length: 3,
      convRule: rule114
  }, {
      start: 69811,
      length: 4,
      convRule: rule84
  }, {
      start: 69815,
      length: 2,
      convRule: rule114
  }, {
      start: 69817,
      length: 2,
      convRule: rule84
  }, {
      start: 69819,
      length: 2,
      convRule: rule2
  }, {
      start: 69821,
      length: 1,
      convRule: rule16
  }, {
      start: 69822,
      length: 4,
      convRule: rule2
  }, {
      start: 73728,
      length: 879,
      convRule: rule45
  }, {
      start: 74752,
      length: 99,
      convRule: rule116
  }, {
      start: 74864,
      length: 4,
      convRule: rule2
  }, {
      start: 77824,
      length: 1071,
      convRule: rule45
  }, {
      start: 92160,
      length: 569,
      convRule: rule45
  }, {
      start: 110592,
      length: 2,
      convRule: rule45
  }, {
      start: 118784,
      length: 246,
      convRule: rule13
  }, {
      start: 119040,
      length: 39,
      convRule: rule13
  }, {
      start: 119081,
      length: 60,
      convRule: rule13
  }, {
      start: 119141,
      length: 2,
      convRule: rule114
  }, {
      start: 119143,
      length: 3,
      convRule: rule84
  }, {
      start: 119146,
      length: 3,
      convRule: rule13
  }, {
      start: 119149,
      length: 6,
      convRule: rule114
  }, {
      start: 119155,
      length: 8,
      convRule: rule16
  }, {
      start: 119163,
      length: 8,
      convRule: rule84
  }, {
      start: 119171,
      length: 2,
      convRule: rule13
  }, {
      start: 119173,
      length: 7,
      convRule: rule84
  }, {
      start: 119180,
      length: 30,
      convRule: rule13
  }, {
      start: 119210,
      length: 4,
      convRule: rule84
  }, {
      start: 119214,
      length: 48,
      convRule: rule13
  }, {
      start: 119296,
      length: 66,
      convRule: rule13
  }, {
      start: 119362,
      length: 3,
      convRule: rule84
  }, {
      start: 119365,
      length: 1,
      convRule: rule13
  }, {
      start: 119552,
      length: 87,
      convRule: rule13
  }, {
      start: 119648,
      length: 18,
      convRule: rule17
  }, {
      start: 119808,
      length: 26,
      convRule: rule98
  }, {
      start: 119834,
      length: 26,
      convRule: rule14
  }, {
      start: 119860,
      length: 26,
      convRule: rule98
  }, {
      start: 119886,
      length: 7,
      convRule: rule14
  }, {
      start: 119894,
      length: 18,
      convRule: rule14
  }, {
      start: 119912,
      length: 26,
      convRule: rule98
  }, {
      start: 119938,
      length: 26,
      convRule: rule14
  }, {
      start: 119964,
      length: 1,
      convRule: rule98
  }, {
      start: 119966,
      length: 2,
      convRule: rule98
  }, {
      start: 119970,
      length: 1,
      convRule: rule98
  }, {
      start: 119973,
      length: 2,
      convRule: rule98
  }, {
      start: 119977,
      length: 4,
      convRule: rule98
  }, {
      start: 119982,
      length: 8,
      convRule: rule98
  }, {
      start: 119990,
      length: 4,
      convRule: rule14
  }, {
      start: 119995,
      length: 1,
      convRule: rule14
  }, {
      start: 119997,
      length: 7,
      convRule: rule14
  }, {
      start: 120005,
      length: 11,
      convRule: rule14
  }, {
      start: 120016,
      length: 26,
      convRule: rule98
  }, {
      start: 120042,
      length: 26,
      convRule: rule14
  }, {
      start: 120068,
      length: 2,
      convRule: rule98
  }, {
      start: 120071,
      length: 4,
      convRule: rule98
  }, {
      start: 120077,
      length: 8,
      convRule: rule98
  }, {
      start: 120086,
      length: 7,
      convRule: rule98
  }, {
      start: 120094,
      length: 26,
      convRule: rule14
  }, {
      start: 120120,
      length: 2,
      convRule: rule98
  }, {
      start: 120123,
      length: 4,
      convRule: rule98
  }, {
      start: 120128,
      length: 5,
      convRule: rule98
  }, {
      start: 120134,
      length: 1,
      convRule: rule98
  }, {
      start: 120138,
      length: 7,
      convRule: rule98
  }, {
      start: 120146,
      length: 26,
      convRule: rule14
  }, {
      start: 120172,
      length: 26,
      convRule: rule98
  }, {
      start: 120198,
      length: 26,
      convRule: rule14
  }, {
      start: 120224,
      length: 26,
      convRule: rule98
  }, {
      start: 120250,
      length: 26,
      convRule: rule14
  }, {
      start: 120276,
      length: 26,
      convRule: rule98
  }, {
      start: 120302,
      length: 26,
      convRule: rule14
  }, {
      start: 120328,
      length: 26,
      convRule: rule98
  }, {
      start: 120354,
      length: 26,
      convRule: rule14
  }, {
      start: 120380,
      length: 26,
      convRule: rule98
  }, {
      start: 120406,
      length: 26,
      convRule: rule14
  }, {
      start: 120432,
      length: 26,
      convRule: rule98
  }, {
      start: 120458,
      length: 28,
      convRule: rule14
  }, {
      start: 120488,
      length: 25,
      convRule: rule98
  }, {
      start: 120513,
      length: 1,
      convRule: rule6
  }, {
      start: 120514,
      length: 25,
      convRule: rule14
  }, {
      start: 120539,
      length: 1,
      convRule: rule6
  }, {
      start: 120540,
      length: 6,
      convRule: rule14
  }, {
      start: 120546,
      length: 25,
      convRule: rule98
  }, {
      start: 120571,
      length: 1,
      convRule: rule6
  }, {
      start: 120572,
      length: 25,
      convRule: rule14
  }, {
      start: 120597,
      length: 1,
      convRule: rule6
  }, {
      start: 120598,
      length: 6,
      convRule: rule14
  }, {
      start: 120604,
      length: 25,
      convRule: rule98
  }, {
      start: 120629,
      length: 1,
      convRule: rule6
  }, {
      start: 120630,
      length: 25,
      convRule: rule14
  }, {
      start: 120655,
      length: 1,
      convRule: rule6
  }, {
      start: 120656,
      length: 6,
      convRule: rule14
  }, {
      start: 120662,
      length: 25,
      convRule: rule98
  }, {
      start: 120687,
      length: 1,
      convRule: rule6
  }, {
      start: 120688,
      length: 25,
      convRule: rule14
  }, {
      start: 120713,
      length: 1,
      convRule: rule6
  }, {
      start: 120714,
      length: 6,
      convRule: rule14
  }, {
      start: 120720,
      length: 25,
      convRule: rule98
  }, {
      start: 120745,
      length: 1,
      convRule: rule6
  }, {
      start: 120746,
      length: 25,
      convRule: rule14
  }, {
      start: 120771,
      length: 1,
      convRule: rule6
  }, {
      start: 120772,
      length: 6,
      convRule: rule14
  }, {
      start: 120778,
      length: 1,
      convRule: rule98
  }, {
      start: 120779,
      length: 1,
      convRule: rule14
  }, {
      start: 120782,
      length: 50,
      convRule: rule8
  }, {
      start: 126976,
      length: 44,
      convRule: rule13
  }, {
      start: 127024,
      length: 100,
      convRule: rule13
  }, {
      start: 127136,
      length: 15,
      convRule: rule13
  }, {
      start: 127153,
      length: 14,
      convRule: rule13
  }, {
      start: 127169,
      length: 15,
      convRule: rule13
  }, {
      start: 127185,
      length: 15,
      convRule: rule13
  }, {
      start: 127232,
      length: 11,
      convRule: rule17
  }, {
      start: 127248,
      length: 31,
      convRule: rule13
  }, {
      start: 127280,
      length: 58,
      convRule: rule13
  }, {
      start: 127344,
      length: 43,
      convRule: rule13
  }, {
      start: 127462,
      length: 29,
      convRule: rule13
  }, {
      start: 127504,
      length: 43,
      convRule: rule13
  }, {
      start: 127552,
      length: 9,
      convRule: rule13
  }, {
      start: 127568,
      length: 2,
      convRule: rule13
  }, {
      start: 127744,
      length: 33,
      convRule: rule13
  }, {
      start: 127792,
      length: 6,
      convRule: rule13
  }, {
      start: 127799,
      length: 70,
      convRule: rule13
  }, {
      start: 127872,
      length: 20,
      convRule: rule13
  }, {
      start: 127904,
      length: 37,
      convRule: rule13
  }, {
      start: 127942,
      length: 5,
      convRule: rule13
  }, {
      start: 127968,
      length: 17,
      convRule: rule13
  }, {
      start: 128000,
      length: 63,
      convRule: rule13
  }, {
      start: 128064,
      length: 1,
      convRule: rule13
  }, {
      start: 128066,
      length: 182,
      convRule: rule13
  }, {
      start: 128249,
      length: 4,
      convRule: rule13
  }, {
      start: 128256,
      length: 62,
      convRule: rule13
  }, {
      start: 128336,
      length: 24,
      convRule: rule13
  }, {
      start: 128507,
      length: 5,
      convRule: rule13
  }, {
      start: 128513,
      length: 16,
      convRule: rule13
  }, {
      start: 128530,
      length: 3,
      convRule: rule13
  }, {
      start: 128534,
      length: 1,
      convRule: rule13
  }, {
      start: 128536,
      length: 1,
      convRule: rule13
  }, {
      start: 128538,
      length: 1,
      convRule: rule13
  }, {
      start: 128540,
      length: 3,
      convRule: rule13
  }, {
      start: 128544,
      length: 6,
      convRule: rule13
  }, {
      start: 128552,
      length: 4,
      convRule: rule13
  }, {
      start: 128557,
      length: 1,
      convRule: rule13
  }, {
      start: 128560,
      length: 4,
      convRule: rule13
  }, {
      start: 128565,
      length: 12,
      convRule: rule13
  }, {
      start: 128581,
      length: 11,
      convRule: rule13
  }, {
      start: 128640,
      length: 70,
      convRule: rule13
  }, {
      start: 128768,
      length: 116,
      convRule: rule13
  }, {
      start: 131072,
      length: 42711,
      convRule: rule45
  }, {
      start: 173824,
      length: 4149,
      convRule: rule45
  }, {
      start: 177984,
      length: 222,
      convRule: rule45
  }, {
      start: 194560,
      length: 542,
      convRule: rule45
  }, {
      start: 917505,
      length: 1,
      convRule: rule16
  }, {
      start: 917536,
      length: 96,
      convRule: rule16
  }, {
      start: 917760,
      length: 240,
      convRule: rule84
  }, {
      start: 983040,
      length: 65534,
      convRule: rule164
  }, {
      start: 1048576,
      length: 65534,
      convRule: rule164
  } ];
  var checkAttr = function (categories) {
      return function ($$char) {
          var numOfBlocks = (function () {
              var $28 = $$char < 256;
              if ($28) {
                  return numLat1Blocks;
              };
              return numBlocks;
          })();
          var maybeConversionRule = getRule(allchars)($$char)(numOfBlocks);
          if (maybeConversionRule instanceof Data_Maybe.Nothing) {
              return false;
          };
          if (maybeConversionRule instanceof Data_Maybe.Just) {
              return Data_Maybe.isJust(Data_Array.elemIndex(Data_Eq.eqInt)(maybeConversionRule.value0.category)(categories));
          };
          throw new Error("Failed pattern match at Data.Char.Unicode.Internal (line 4800, column 8 - line 4802, column 92): " + [ maybeConversionRule.constructor.name ]);
      };
  };                                                                                                                                          
  var uIswalpha = checkAttr([ gencatLL, gencatLU, gencatLT, gencatLM, gencatLO ]);                                                                                                                                                                                    
  var uIswupper = checkAttr([ gencatLU, gencatLT ]);
  exports["uIswupper"] = uIswupper;
  exports["uIswalpha"] = uIswalpha;
  exports["uIswspace"] = uIswspace;
  exports["uTowupper"] = uTowupper;
  exports["uTowlower"] = uTowlower;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Char.Unicode"] = $PS["Data.Char.Unicode"] || {};
  var exports = $PS["Data.Char.Unicode"];
  var $foreign = $PS["Data.Char.Unicode"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Char = $PS["Data.Char"];
  var Data_Char_Unicode_Internal = $PS["Data.Char.Unicode.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var toUpper = $foreign.withCharCode(Data_Char_Unicode_Internal.uTowupper);
  var toLower = $foreign.withCharCode(Data_Char_Unicode_Internal.uTowlower);
  var isUpper = function ($50) {
      return Data_Char_Unicode_Internal.uIswupper(Data_Char.toCharCode($50));
  };
  var isSpace = function (c) {
      var uc = Data_Char.toCharCode(c);
      var $14 = uc <= 823;
      if ($14) {
          return uc === 32 || (uc >= 9 && uc <= 13 || uc === 160);
      };
      return Data_Char_Unicode_Internal.uIswspace(Data_Char.toCharCode(c));
  };
  var isOctDigit = function (c) {
      var diff = Data_Char.toCharCode(c) - Data_Char.toCharCode("0") | 0;
      return diff <= 7 && diff >= 0;
  };
  var isDigit = function (c) {
      var diff = Data_Char.toCharCode(c) - Data_Char.toCharCode("0") | 0;
      return diff <= 9 && diff >= 0;
  };
  var isHexDigit = function (c) {
      return isDigit(c) || ((function () {
          var diff = Data_Char.toCharCode(c) - Data_Char.toCharCode("A") | 0;
          return diff <= 5 && diff >= 0;
      })() || (function () {
          var diff = Data_Char.toCharCode(c) - Data_Char.toCharCode("a") | 0;
          return diff <= 5 && diff >= 0;
      })());
  };
  var isAlpha = function ($55) {
      return Data_Char_Unicode_Internal.uIswalpha(Data_Char.toCharCode($55));
  }; 
  var digitToInt = function (c) {
      var hexUpper = Data_Char.toCharCode(c) - Data_Char.toCharCode("A") | 0;
      var hexLower = Data_Char.toCharCode(c) - Data_Char.toCharCode("a") | 0;
      var dec = Data_Char.toCharCode(c) - Data_Char.toCharCode("0") | 0;
      var result = (function () {
          if (dec <= 9 && dec >= 0) {
              return new Data_Maybe.Just(dec);
          };
          if (hexLower <= 5 && hexLower >= 0) {
              return Data_Maybe.Just.create(hexLower + 10 | 0);
          };
          if (hexUpper <= 5 && hexUpper >= 0) {
              return Data_Maybe.Just.create(hexUpper + 10 | 0);
          };
          if (Data_Boolean.otherwise) {
              return Data_Maybe.Nothing.value;
          };
          throw new Error("Failed pattern match at Data.Char.Unicode (line 547, column 5 - line 547, column 24): " + [  ]);
      })();
      return result;
  };
  exports["isUpper"] = isUpper;
  exports["isAlpha"] = isAlpha;
  exports["isDigit"] = isDigit;
  exports["isOctDigit"] = isOctDigit;
  exports["isHexDigit"] = isHexDigit;
  exports["isSpace"] = isSpace;
  exports["digitToInt"] = digitToInt;
  exports["toLower"] = toLower;
  exports["toUpper"] = toUpper;
})(PS);
(function(exports) {
  "use strict";

  exports.intSub = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x - y | 0;
    };
  };
})(PS["Data.Ring"] = PS["Data.Ring"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Ring"] = $PS["Data.Ring"] || {};
  var exports = $PS["Data.Ring"];
  var $foreign = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Ring = function (Semiring0, sub) {
      this.Semiring0 = Semiring0;
      this.sub = sub;
  };
  var sub = function (dict) {
      return dict.sub;
  };                  
  var ringInt = new Ring(function () {
      return Data_Semiring.semiringInt;
  }, $foreign.intSub);
  var negate = function (dictRing) {
      return function (a) {
          return sub(dictRing)(Data_Semiring.zero(dictRing.Semiring0()))(a);
      };
  };
  exports["Ring"] = Ring;
  exports["negate"] = negate;
  exports["ringInt"] = ringInt;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.CommutativeRing"] = $PS["Data.CommutativeRing"] || {};
  var exports = $PS["Data.CommutativeRing"];
  var Data_Ring = $PS["Data.Ring"];
  var CommutativeRing = function (Ring0) {
      this.Ring0 = Ring0;
  }; 
  var commutativeRingInt = new CommutativeRing(function () {
      return Data_Ring.ringInt;
  });
  exports["CommutativeRing"] = CommutativeRing;
  exports["commutativeRingInt"] = commutativeRingInt;
})(PS);
(function(exports) {
  
  var Decimal =require("decimal.js"); 

  Decimal.set({ precision: 30 });
  Decimal.set({ modulo: Decimal.EUCLID });

  exports.fromInt = function(x) {
    return new Decimal(x);
  };

  exports["fromString'"] = function(nothing) {
    return function(just) {
      return function(str) {
        try {
          return just(new Decimal(str));
        }
        catch (e) {
          return nothing;
        }
      };
    };
  };

  exports.fromNumber = function(x) {
    return new Decimal(x);
  };

  exports.toNumber = function(x) {
    return x.toNumber();
  };

  exports.toString = function(x) {
    return x.toString();
  };

  exports.isInteger = function(x) {
    return x.isInteger();
  };

  exports.isFinite = function(x) {
    return x.isFinite();
  };

  exports.toSignificantDigits = function(d) {
    return function (x) {
      return x.toSignificantDigits(d);
    };
  };

  exports.dAdd = function(x) {
    return function(y) {
      return x.add(y);
    };
  };

  exports.modulo = function(x) {
    return function(y) {
      return x.mod(y);
    };
  };

  exports.dMul = function(x) {
    return function(y) {
      return x.mul(y);
    };
  };

  exports.dSub = function(x) {
    return function(y) {
      return x.minus(y);
    };
  };

  exports.dDiv = function(x) {
    return function(y) {
      return x.div(y);
    };
  };

  exports.dEquals = function(x) {
    return function(y) {
      return x.equals(y);
    };
  };

  exports.dCompare = function(x) {
    return function(y) {
      return x.cmp(y);
    };
  };

  exports.abs = function(x) {
    return x.abs();
  };

  exports.pow = function(x) {
    return function(y) {
      return x.pow(y);
    };
  };

  exports.exp = function(x) {
    return x.exp();
  };

  exports.acos = function(x) {
    return x.acos();
  };

  exports.abs = function(x) {
    return x.abs();
  };

  exports.acos = function(x) {
    return x.acos();
  };

  exports.acosh = function(x) {
    return x.acosh();
  };

  exports.asin = function(x) {
    return x.asin();
  };

  exports.asinh = function(x) {
    return x.asinh();
  };

  exports.atan = function(x) {
    return x.atan();
  };

  exports.atanh = function(x) {
    return x.atanh();
  };

  exports.atan2 = function(x) {
    return function(y) {
      return Decimal.atan2(x, y);
    };
  };

  exports.ceil = function(x) {
    return x.ceil();
  };

  exports.cos = function(x) {
    return x.cos();
  };

  exports.cosh = function(x) {
    return x.cosh();
  };

  exports.exp = function(x) {
    return x.exp();
  };

  exports.floor = function(x) {
    return x.floor();
  };

  exports.ln = function(x) {
    return x.ln();
  };

  exports.log10 = function(x) {
    return Decimal.log10(x);
  };

  exports.max = function(x) {
    return function(y) {
      return Decimal.max(x, y);
    };
  };

  exports.min = function(x) {
    return function(y) {
      return Decimal.min(x, y);
    };
  };

  exports.round = function(x) {
    return x.round();
  };

  exports.sin = function(x) {
    return x.sin();
  };

  exports.sinh = function(x) {
    return x.sinh();
  };

  exports.tan = function(x) {
    return x.tan();
  };

  exports.tanh = function(x) {
    return x.tanh();
  };

  exports.e = Decimal.exp(1.0);

  exports.pi = new Decimal('3.14159265358979323846264338327950288419716939937510582097494459230781640628620899');

  var p = [
    676.5203681218851,
    -1259.1392167224028,
    771.32342877765313,
    -176.61502916214059,
    12.507343278686905,
    -0.13857109526572012,
    9.9843695780195716e-6,
    1.5056327351493116e-7,
  ];

  exports.gamma = function(z) {
    var pval, i, x, y, t, zr;
    var one = new Decimal(1.0);
    if (z < 0.5) {
      // Reflection formula)
      y = Decimal.div(
            exports.pi,
            Decimal.mul(Decimal.sin(exports.pi.mul(z)), exports.gamma(one.sub(z))));
    }
    else {
      zr = z.sub(one);
      x = new Decimal(0.99999999999980993);
      i = 0;
      for (i = 0; i < p.length; i++) {
        pval = p[i];
        x = x.add(Decimal.div(pval, zr.add(i).add(one)));
      }
      t = zr.add(p.length).sub(0.5);
      y = Decimal.sqrt(exports.pi.mul(2.0))
            .mul(Decimal.pow(t, zr.add(0.5)))
            .mul(Decimal.exp(t.neg()))
            .mul(x);
    }
    if (z.isInteger()) {
      return y.round();
    }
    return y;
  };
})(PS["Data.Decimal"] = PS["Data.Decimal"] || {});
(function(exports) {
  "use strict";

  exports.intDegree = function (x) {
    return Math.min(Math.abs(x), 2147483647);
  };

  // See the Euclidean definition in
  // https://en.m.wikipedia.org/wiki/Modulo_operation.
  exports.intDiv = function (x) {
    return function (y) {
      if (y === 0) return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };

  exports.intMod = function (x) {
    return function (y) {
      if (y === 0) return 0;
      var yy = Math.abs(y);
      return ((x % yy) + yy) % yy;
    };
  };
})(PS["Data.EuclideanRing"] = PS["Data.EuclideanRing"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.EuclideanRing"] = $PS["Data.EuclideanRing"] || {};
  var exports = $PS["Data.EuclideanRing"];
  var $foreign = $PS["Data.EuclideanRing"];
  var Data_CommutativeRing = $PS["Data.CommutativeRing"];  
  var EuclideanRing = function (CommutativeRing0, degree, div, mod) {
      this.CommutativeRing0 = CommutativeRing0;
      this.degree = degree;
      this.div = div;
      this.mod = mod;
  };
  var mod = function (dict) {
      return dict.mod;
  }; 
  var euclideanRingInt = new EuclideanRing(function () {
      return Data_CommutativeRing.commutativeRingInt;
  }, $foreign.intDegree, $foreign.intDiv, $foreign.intMod);
  var div = function (dict) {
      return dict.div;
  };
  exports["EuclideanRing"] = EuclideanRing;
  exports["div"] = div;
  exports["mod"] = mod;
  exports["euclideanRingInt"] = euclideanRingInt;
})(PS);
(function(exports) {
  "use strict";

  exports.showIntImpl = function (n) {
    return n.toString();
  };

  exports.showNumberImpl = function (n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };

  exports.showCharImpl = function (c) {
    var code = c.charCodeAt(0);
    if (code < 0x20 || code === 0x7F) {
      switch (c) {
        case "\x07": return "'\\a'";
        case "\b": return "'\\b'";
        case "\f": return "'\\f'";
        case "\n": return "'\\n'";
        case "\r": return "'\\r'";
        case "\t": return "'\\t'";
        case "\v": return "'\\v'";
      }
      return "'\\" + code.toString(10) + "'";
    }
    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };

  exports.showStringImpl = function (s) {
    var l = s.length;
    return "\"" + s.replace(
      /[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
      function (c, i) {
        switch (c) {
          case "\"":
          case "\\":
            return "\\" + c;
          case "\x07": return "\\a";
          case "\b": return "\\b";
          case "\f": return "\\f";
          case "\n": return "\\n";
          case "\r": return "\\r";
          case "\t": return "\\t";
          case "\v": return "\\v";
        }
        var k = i + 1;
        var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty;
      }
    ) + "\"";
  };

  exports.showArrayImpl = function (f) {
    return function (xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Show"] = $PS["Data.Show"] || {};
  var exports = $PS["Data.Show"];
  var $foreign = $PS["Data.Show"];
  var Show = function (show) {
      this.show = show;
  };
  var showString = new Show($foreign.showStringImpl);
  var showNumber = new Show($foreign.showNumberImpl);
  var showInt = new Show($foreign.showIntImpl);
  var showChar = new Show($foreign.showCharImpl);
  var show = function (dict) {
      return dict.show;
  };
  var showArray = function (dictShow) {
      return new Show($foreign.showArrayImpl(show(dictShow)));
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showInt"] = showInt;
  exports["showNumber"] = showNumber;
  exports["showChar"] = showChar;
  exports["showString"] = showString;
  exports["showArray"] = showArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Decimal"] = $PS["Data.Decimal"] || {};
  var exports = $PS["Data.Decimal"];
  var $foreign = $PS["Data.Decimal"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_CommutativeRing = $PS["Data.CommutativeRing"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Show = $PS["Data.Show"];                
  var showDecimal = new Data_Show.Show(function (x) {
      return "(fromString \"" + ($foreign.toString(x) + "\")");
  });
  var semiringDecimal = new Data_Semiring.Semiring($foreign.dAdd, $foreign.dMul, $foreign.fromInt(1), $foreign.fromInt(0));
  var ringDecimal = new Data_Ring.Ring(function () {
      return semiringDecimal;
  }, $foreign.dSub);
  var fromString = $foreign["fromString'"](Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
  var eqDecimal = new Data_Eq.Eq($foreign.dEquals);
  var ordDecimal = new Data_Ord.Ord(function () {
      return eqDecimal;
  }, function (x) {
      return function (y) {
          var v = $foreign.dCompare(x)(y);
          if (v === 1) {
              return Data_Ordering.GT.value;
          };
          if (v === 0) {
              return Data_Ordering.EQ.value;
          };
          return Data_Ordering.LT.value;
      };
  });
  var commutativeRingDecimal = new Data_CommutativeRing.CommutativeRing(function () {
      return ringDecimal;
  });
  var euclideanRingDecimal = new Data_EuclideanRing.EuclideanRing(function () {
      return commutativeRingDecimal;
  }, function (v) {
      return 1;
  }, $foreign.dDiv, function (v) {
      return function (v1) {
          return Data_Semiring.zero(semiringDecimal);
      };
  });
  var factorial = function (n) {
      if (Data_Ord.lessThan(ordDecimal)(n)(Data_Semiring.zero(semiringDecimal))) {
          return Data_EuclideanRing.div(euclideanRingDecimal)(Data_Semiring.one(semiringDecimal))(Data_Semiring.zero(semiringDecimal));
      };
      if (Data_Boolean.otherwise) {
          return $foreign.gamma($foreign.ceil(Data_Semiring.add(semiringDecimal)(n)(Data_Semiring.one(semiringDecimal))));
      };
      throw new Error("Failed pattern match at Data.Decimal (line 231, column 1 - line 231, column 30): " + [ n.constructor.name ]);
  };
  exports["fromString"] = fromString;
  exports["factorial"] = factorial;
  exports["eqDecimal"] = eqDecimal;
  exports["ordDecimal"] = ordDecimal;
  exports["showDecimal"] = showDecimal;
  exports["semiringDecimal"] = semiringDecimal;
  exports["ringDecimal"] = ringDecimal;
  exports["euclideanRingDecimal"] = euclideanRingDecimal;
  exports["fromInt"] = $foreign.fromInt;
  exports["fromNumber"] = $foreign.fromNumber;
  exports["toNumber"] = $foreign.toNumber;
  exports["toString"] = $foreign.toString;
  exports["isFinite"] = $foreign["isFinite"];
  exports["isInteger"] = $foreign.isInteger;
  exports["toSignificantDigits"] = $foreign.toSignificantDigits;
  exports["abs"] = $foreign.abs;
  exports["acos"] = $foreign.acos;
  exports["acosh"] = $foreign.acosh;
  exports["asin"] = $foreign.asin;
  exports["asinh"] = $foreign.asinh;
  exports["atan"] = $foreign.atan;
  exports["atan2"] = $foreign.atan2;
  exports["atanh"] = $foreign.atanh;
  exports["ceil"] = $foreign.ceil;
  exports["cos"] = $foreign.cos;
  exports["cosh"] = $foreign.cosh;
  exports["exp"] = $foreign.exp;
  exports["floor"] = $foreign.floor;
  exports["ln"] = $foreign.ln;
  exports["log10"] = $foreign.log10;
  exports["max"] = $foreign.max;
  exports["min"] = $foreign.min;
  exports["modulo"] = $foreign.modulo;
  exports["pow"] = $foreign.pow;
  exports["round"] = $foreign.round;
  exports["sin"] = $foreign.sin;
  exports["sinh"] = $foreign.sinh;
  exports["tan"] = $foreign.tan;
  exports["tanh"] = $foreign.tanh;
  exports["e"] = $foreign.e;
  exports["pi"] = $foreign.pi;
  exports["gamma"] = $foreign.gamma;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Identity"] = $PS["Data.Identity"] || {};
  var exports = $PS["Data.Identity"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Newtype = $PS["Data.Newtype"];          
  var Identity = function (x) {
      return x;
  };
  var newtypeIdentity = new Data_Newtype.Newtype(function (n) {
      return n;
  }, Identity);
  var functorIdentity = new Data_Functor.Functor(function (f) {
      return function (m) {
          return f(m);
      };
  });
  var applyIdentity = new Control_Apply.Apply(function () {
      return functorIdentity;
  }, function (v) {
      return function (v1) {
          return v(v1);
      };
  });
  var bindIdentity = new Control_Bind.Bind(function () {
      return applyIdentity;
  }, function (v) {
      return function (f) {
          return f(v);
      };
  });
  var applicativeIdentity = new Control_Applicative.Applicative(function () {
      return applyIdentity;
  }, Identity);
  var monadIdentity = new Control_Monad.Monad(function () {
      return applicativeIdentity;
  }, function () {
      return bindIdentity;
  });
  exports["newtypeIdentity"] = newtypeIdentity;
  exports["functorIdentity"] = functorIdentity;
  exports["monadIdentity"] = monadIdentity;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Traversable"] = $PS["Data.Traversable"] || {};
  var exports = $PS["Data.Traversable"];                                                       
  var Traversable = function (Foldable1, Functor0, sequence, traverse) {
      this.Foldable1 = Foldable1;
      this.Functor0 = Functor0;
      this.sequence = sequence;
      this.traverse = traverse;
  };
  var traverse = function (dict) {
      return dict.traverse;
  }; 
  var sequence = function (dict) {
      return dict.sequence;
  };
  exports["Traversable"] = Traversable;
  exports["traverse"] = traverse;
  exports["sequence"] = sequence;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.NonEmpty"] = $PS["Data.NonEmpty"] || {};
  var exports = $PS["Data.NonEmpty"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Traversable = $PS["Data.Traversable"];                
  var NonEmpty = (function () {
      function NonEmpty(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      NonEmpty.create = function (value0) {
          return function (value1) {
              return new NonEmpty(value0, value1);
          };
      };
      return NonEmpty;
  })();
  var functorNonEmpty = function (dictFunctor) {
      return new Data_Functor.Functor(function (f) {
          return function (m) {
              return new NonEmpty(f(m.value0), Data_Functor.map(dictFunctor)(f)(m.value1));
          };
      });
  };
  var foldl1 = function (dictFoldable) {
      return function (f) {
          return function (v) {
              return Data_Foldable.foldl(dictFoldable)(f)(v.value0)(v.value1);
          };
      };
  };
  var foldableNonEmpty = function (dictFoldable) {
      return new Data_Foldable.Foldable(function (dictMonoid) {
          return function (f) {
              return function (v) {
                  return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f)(v.value1));
              };
          };
      }, function (f) {
          return function (b) {
              return function (v) {
                  return Data_Foldable.foldl(dictFoldable)(f)(f(b)(v.value0))(v.value1);
              };
          };
      }, function (f) {
          return function (b) {
              return function (v) {
                  return f(v.value0)(Data_Foldable.foldr(dictFoldable)(f)(b)(v.value1));
              };
          };
      });
  };
  var traversableNonEmpty = function (dictTraversable) {
      return new Data_Traversable.Traversable(function () {
          return foldableNonEmpty(dictTraversable.Foldable1());
      }, function () {
          return functorNonEmpty(dictTraversable.Functor0());
      }, function (dictApplicative) {
          return function (v) {
              return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(v.value0))(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v.value1));
          };
      }, function (dictApplicative) {
          return function (f) {
              return function (v) {
                  return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(f(v.value0)))(Data_Traversable.traverse(dictTraversable)(dictApplicative)(f)(v.value1));
              };
          };
      });
  };
  exports["NonEmpty"] = NonEmpty;
  exports["foldl1"] = foldl1;
  exports["functorNonEmpty"] = functorNonEmpty;
  exports["foldableNonEmpty"] = foldableNonEmpty;
  exports["traversableNonEmpty"] = traversableNonEmpty;
})(PS);
(function(exports) {
  "use strict";

  exports.unfoldrArrayImpl = function (isNothing) {
    return function (fromJust) {
      return function (fst) {
        return function (snd) {
          return function (f) {
            return function (b) {
              var result = [];
              var value = b;
              while (true) { // eslint-disable-line no-constant-condition
                var maybe = f(value);
                if (isNothing(maybe)) return result;
                var tuple = fromJust(maybe);
                result.push(fst(tuple));
                value = snd(tuple);
              }
            };
          };
        };
      };
    };
  };
})(PS["Data.Unfoldable"] = PS["Data.Unfoldable"] || {});
(function(exports) {
  "use strict";

  exports.unfoldr1ArrayImpl = function (isNothing) {
    return function (fromJust) {
      return function (fst) {
        return function (snd) {
          return function (f) {
            return function (b) {
              var result = [];
              var value = b;
              while (true) { // eslint-disable-line no-constant-condition
                var tuple = f(value);
                result.push(fst(tuple));
                var maybe = snd(tuple);
                if (isNothing(maybe)) return result;
                value = fromJust(maybe);
              }
            };
          };
        };
      };
    };
  };
})(PS["Data.Unfoldable1"] = PS["Data.Unfoldable1"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Unfoldable1"] = $PS["Data.Unfoldable1"] || {};
  var exports = $PS["Data.Unfoldable1"];
  var $foreign = $PS["Data.Unfoldable1"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];                
  var Unfoldable1 = function (unfoldr1) {
      this.unfoldr1 = unfoldr1;
  };
  var unfoldable1Array = new Unfoldable1($foreign.unfoldr1ArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
  exports["Unfoldable1"] = Unfoldable1;
  exports["unfoldable1Array"] = unfoldable1Array;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Unfoldable"] = $PS["Data.Unfoldable"] || {};
  var exports = $PS["Data.Unfoldable"];
  var $foreign = $PS["Data.Unfoldable"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable1 = $PS["Data.Unfoldable1"];  
  var Unfoldable = function (Unfoldable10, unfoldr) {
      this.Unfoldable10 = Unfoldable10;
      this.unfoldr = unfoldr;
  };
  var unfoldr = function (dict) {
      return dict.unfoldr;
  };
  var unfoldableArray = new Unfoldable(function () {
      return Data_Unfoldable1.unfoldable1Array;
  }, $foreign.unfoldrArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
  exports["Unfoldable"] = Unfoldable;
  exports["unfoldr"] = unfoldr;
  exports["unfoldableArray"] = unfoldableArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.List.Types"] = $PS["Data.List.Types"] || {};
  var exports = $PS["Data.List.Types"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];
  var Data_Unfoldable1 = $PS["Data.Unfoldable1"];                
  var Nil = (function () {
      function Nil() {

      };
      Nil.value = new Nil();
      return Nil;
  })();
  var Cons = (function () {
      function Cons(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Cons.create = function (value0) {
          return function (value1) {
              return new Cons(value0, value1);
          };
      };
      return Cons;
  })();
  var NonEmptyList = function (x) {
      return x;
  };
  var listMap = function (f) {
      var chunkedRevMap = function ($copy_chunksAcc) {
          return function ($copy_v) {
              var $tco_var_chunksAcc = $copy_chunksAcc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(chunksAcc, v) {
                  if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
                      $tco_var_chunksAcc = new Cons(v, chunksAcc);
                      $copy_v = v.value1.value1.value1;
                      return;
                  };
                  var unrolledMap = function (v1) {
                      if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
                          return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
                      };
                      if (v1 instanceof Cons && v1.value1 instanceof Nil) {
                          return new Cons(f(v1.value0), Nil.value);
                      };
                      return Nil.value;
                  };
                  var reverseUnrolledMap = function ($copy_v1) {
                      return function ($copy_acc) {
                          var $tco_var_v1 = $copy_v1;
                          var $tco_done = false;
                          var $tco_result;
                          function $tco_loop(v1, acc) {
                              if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                                  $tco_var_v1 = v1.value1;
                                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                                  return;
                              };
                              $tco_done = true;
                              return acc;
                          };
                          while (!$tco_done) {
                              $tco_result = $tco_loop($tco_var_v1, $copy_acc);
                          };
                          return $tco_result;
                      };
                  };
                  $tco_done = true;
                  return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
              };
              return $tco_result;
          };
      };
      return chunkedRevMap(Nil.value);
  };
  var functorList = new Data_Functor.Functor(listMap);
  var functorNonEmptyList = Data_NonEmpty.functorNonEmpty(functorList);
  var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
      return function (f) {
          return Data_Foldable.foldl(foldableList)(function (acc) {
              var $202 = Data_Semigroup.append(dictMonoid.Semigroup0())(acc);
              return function ($203) {
                  return $202(f($203));
              };
          })(Data_Monoid.mempty(dictMonoid));
      };
  }, function (f) {
      var go = function ($copy_b) {
          return function ($copy_v) {
              var $tco_var_b = $copy_b;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(b, v) {
                  if (v instanceof Nil) {
                      $tco_done = true;
                      return b;
                  };
                  if (v instanceof Cons) {
                      $tco_var_b = f(b)(v.value0);
                      $copy_v = v.value1;
                      return;
                  };
                  throw new Error("Failed pattern match at Data.List.Types (line 109, column 12 - line 111, column 30): " + [ v.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_b, $copy_v);
              };
              return $tco_result;
          };
      };
      return go;
  }, function (f) {
      return function (b) {
          var rev = Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value);
          var $204 = Data_Foldable.foldl(foldableList)(Data_Function.flip(f))(b);
          return function ($205) {
              return $204(rev($205));
          };
      };
  });
  var foldableNonEmptyList = Data_NonEmpty.foldableNonEmpty(foldableList);
  var semigroupList = new Data_Semigroup.Semigroup(function (xs) {
      return function (ys) {
          return Data_Foldable.foldr(foldableList)(Cons.create)(ys)(xs);
      };
  });
  var traversableList = new Data_Traversable.Traversable(function () {
      return foldableList;
  }, function () {
      return functorList;
  }, function (dictApplicative) {
      return Data_Traversable.traverse(traversableList)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
  }, function (dictApplicative) {
      return function (f) {
          var $219 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value));
          var $220 = Data_Foldable.foldl(foldableList)(function (acc) {
              var $222 = Control_Apply.lift2(dictApplicative.Apply0())(Data_Function.flip(Cons.create))(acc);
              return function ($223) {
                  return $222(f($223));
              };
          })(Control_Applicative.pure(dictApplicative)(Nil.value));
          return function ($221) {
              return $219($220($221));
          };
      };
  });
  var unfoldable1List = new Data_Unfoldable1.Unfoldable1(function (f) {
      return function (b) {
          var go = function ($copy_source) {
              return function ($copy_memo) {
                  var $tco_var_source = $copy_source;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(source, memo) {
                      var v = f(source);
                      if (v.value1 instanceof Data_Maybe.Just) {
                          $tco_var_source = v.value1.value0;
                          $copy_memo = new Cons(v.value0, memo);
                          return;
                      };
                      if (v.value1 instanceof Data_Maybe.Nothing) {
                          $tco_done = true;
                          return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
                      };
                      throw new Error("Failed pattern match at Data.List.Types (line 133, column 22 - line 135, column 61): " + [ v.constructor.name ]);
                  };
                  while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_source, $copy_memo);
                  };
                  return $tco_result;
              };
          };
          return go(b)(Nil.value);
      };
  });
  var unfoldableList = new Data_Unfoldable.Unfoldable(function () {
      return unfoldable1List;
  }, function (f) {
      return function (b) {
          var go = function ($copy_source) {
              return function ($copy_memo) {
                  var $tco_var_source = $copy_source;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(source, memo) {
                      var v = f(source);
                      if (v instanceof Data_Maybe.Nothing) {
                          $tco_done = true;
                          return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(memo);
                      };
                      if (v instanceof Data_Maybe.Just) {
                          $tco_var_source = v.value0.value1;
                          $copy_memo = new Cons(v.value0.value0, memo);
                          return;
                      };
                      throw new Error("Failed pattern match at Data.List.Types (line 140, column 22 - line 142, column 52): " + [ v.constructor.name ]);
                  };
                  while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_source, $copy_memo);
                  };
                  return $tco_result;
              };
          };
          return go(b)(Nil.value);
      };
  });
  var eq1List = new Data_Eq.Eq1(function (dictEq) {
      return function (xs) {
          return function (ys) {
              var go = function ($copy_v) {
                  return function ($copy_v1) {
                      return function ($copy_v2) {
                          var $tco_var_v = $copy_v;
                          var $tco_var_v1 = $copy_v1;
                          var $tco_done = false;
                          var $tco_result;
                          function $tco_loop(v, v1, v2) {
                              if (!v2) {
                                  $tco_done = true;
                                  return false;
                              };
                              if (v instanceof Nil && v1 instanceof Nil) {
                                  $tco_done = true;
                                  return v2;
                              };
                              if (v instanceof Cons && v1 instanceof Cons) {
                                  $tco_var_v = v.value1;
                                  $tco_var_v1 = v1.value1;
                                  $copy_v2 = v2 && Data_Eq.eq(dictEq)(v1.value0)(v.value0);
                                  return;
                              };
                              $tco_done = true;
                              return false;
                          };
                          while (!$tco_done) {
                              $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
                          };
                          return $tco_result;
                      };
                  };
              };
              return go(xs)(ys)(true);
          };
      };
  });
  var eqList = function (dictEq) {
      return new Data_Eq.Eq(Data_Eq.eq1(eq1List)(dictEq));
  }; 
  var applyList = new Control_Apply.Apply(function () {
      return functorList;
  }, function (v) {
      return function (v1) {
          if (v instanceof Nil) {
              return Nil.value;
          };
          if (v instanceof Cons) {
              return Data_Semigroup.append(semigroupList)(Data_Functor.map(functorList)(v.value0)(v1))(Control_Apply.apply(applyList)(v.value1)(v1));
          };
          throw new Error("Failed pattern match at Data.List.Types (line 155, column 1 - line 157, column 48): " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var bindList = new Control_Bind.Bind(function () {
      return applyList;
  }, function (v) {
      return function (v1) {
          if (v instanceof Nil) {
              return Nil.value;
          };
          if (v instanceof Cons) {
              return Data_Semigroup.append(semigroupList)(v1(v.value0))(Control_Bind.bind(bindList)(v.value1)(v1));
          };
          throw new Error("Failed pattern match at Data.List.Types (line 162, column 1 - line 164, column 37): " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var applicativeList = new Control_Applicative.Applicative(function () {
      return applyList;
  }, function (a) {
      return new Cons(a, Nil.value);
  });
  exports["Nil"] = Nil;
  exports["Cons"] = Cons;
  exports["NonEmptyList"] = NonEmptyList;
  exports["eqList"] = eqList;
  exports["semigroupList"] = semigroupList;
  exports["functorList"] = functorList;
  exports["foldableList"] = foldableList;
  exports["unfoldableList"] = unfoldableList;
  exports["traversableList"] = traversableList;
  exports["applicativeList"] = applicativeList;
  exports["bindList"] = bindList;
  exports["functorNonEmptyList"] = functorNonEmptyList;
  exports["foldableNonEmptyList"] = foldableNonEmptyList;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.List"] = $PS["Data.List"] || {};
  var exports = $PS["Data.List"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];                                  
  var uncons = function (v) {
      if (v instanceof Data_List_Types.Nil) {
          return Data_Maybe.Nothing.value;
      };
      if (v instanceof Data_List_Types.Cons) {
          return new Data_Maybe.Just({
              head: v.value0,
              tail: v.value1
          });
      };
      throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [ v.constructor.name ]);
  };
  var toUnfoldable = function (dictUnfoldable) {
      return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
          return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
              return new Data_Tuple.Tuple(rec.head, rec.tail);
          })(uncons(xs));
      });
  };
  var tail = function (v) {
      if (v instanceof Data_List_Types.Nil) {
          return Data_Maybe.Nothing.value;
      };
      if (v instanceof Data_List_Types.Cons) {
          return new Data_Maybe.Just(v.value1);
      };
      throw new Error("Failed pattern match at Data.List (line 245, column 1 - line 245, column 43): " + [ v.constructor.name ]);
  };
  var span = function (v) {
      return function (v1) {
          if (v1 instanceof Data_List_Types.Cons && v(v1.value0)) {
              var v2 = span(v)(v1.value1);
              return {
                  init: new Data_List_Types.Cons(v1.value0, v2.init),
                  rest: v2.rest
              };
          };
          return {
              init: Data_List_Types.Nil.value,
              rest: v1
          };
      };
  };
  var singleton = function (a) {
      return new Data_List_Types.Cons(a, Data_List_Types.Nil.value);
  };
  var sortBy = function (cmp) {
      var merge = function (v) {
          return function (v1) {
              if (v instanceof Data_List_Types.Cons && v1 instanceof Data_List_Types.Cons) {
                  if (Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1.value0))(Data_Ordering.GT.value)) {
                      return new Data_List_Types.Cons(v1.value0, merge(v)(v1.value1));
                  };
                  if (Data_Boolean.otherwise) {
                      return new Data_List_Types.Cons(v.value0, merge(v.value1)(v1));
                  };
              };
              if (v instanceof Data_List_Types.Nil) {
                  return v1;
              };
              if (v1 instanceof Data_List_Types.Nil) {
                  return v;
              };
              throw new Error("Failed pattern match at Data.List (line 473, column 3 - line 473, column 38): " + [ v.constructor.name, v1.constructor.name ]);
          };
      };
      var mergePairs = function (v) {
          if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Cons) {
              return new Data_List_Types.Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
          };
          return v;
      };
      var mergeAll = function ($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
              if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
                  $tco_done = true;
                  return v.value0;
              };
              $copy_v = mergePairs(v);
              return;
          };
          while (!$tco_done) {
              $tco_result = $tco_loop($copy_v);
          };
          return $tco_result;
      };
      var sequences = function (v) {
          if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Cons) {
              if (Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v.value1.value0))(Data_Ordering.GT.value)) {
                  return descending(v.value1.value0)(singleton(v.value0))(v.value1.value1);
              };
              if (Data_Boolean.otherwise) {
                  return ascending(v.value1.value0)(function (v1) {
                      return new Data_List_Types.Cons(v.value0, v1);
                  })(v.value1.value1);
              };
          };
          return singleton(v);
      };
      var descending = function ($copy_a) {
          return function ($copy_as) {
              return function ($copy_v) {
                  var $tco_var_a = $copy_a;
                  var $tco_var_as = $copy_as;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(a, as, v) {
                      if (v instanceof Data_List_Types.Cons && Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(a)(v.value0))(Data_Ordering.GT.value)) {
                          $tco_var_a = v.value0;
                          $tco_var_as = new Data_List_Types.Cons(a, as);
                          $copy_v = v.value1;
                          return;
                      };
                      $tco_done = true;
                      return new Data_List_Types.Cons(new Data_List_Types.Cons(a, as), sequences(v));
                  };
                  while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_a, $tco_var_as, $copy_v);
                  };
                  return $tco_result;
              };
          };
      };
      var ascending = function ($copy_a) {
          return function ($copy_as) {
              return function ($copy_v) {
                  var $tco_var_a = $copy_a;
                  var $tco_var_as = $copy_as;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(a, as, v) {
                      if (v instanceof Data_List_Types.Cons && Data_Eq.notEq(Data_Ordering.eqOrdering)(cmp(a)(v.value0))(Data_Ordering.GT.value)) {
                          $tco_var_a = v.value0;
                          $tco_var_as = function (ys) {
                              return as(new Data_List_Types.Cons(a, ys));
                          };
                          $copy_v = v.value1;
                          return;
                      };
                      $tco_done = true;
                      return new Data_List_Types.Cons(as(singleton(a)), sequences(v));
                  };
                  while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_a, $tco_var_as, $copy_v);
                  };
                  return $tco_result;
              };
          };
      };
      return function ($331) {
          return mergeAll(sequences($331));
      };
  };
  var reverse = (function () {
      var go = function ($copy_acc) {
          return function ($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                  if (v instanceof Data_List_Types.Nil) {
                      $tco_done = true;
                      return acc;
                  };
                  if (v instanceof Data_List_Types.Cons) {
                      $tco_var_acc = new Data_List_Types.Cons(v.value0, acc);
                      $copy_v = v.value1;
                      return;
                  };
                  throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [ acc.constructor.name, v.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_acc, $copy_v);
              };
              return $tco_result;
          };
      };
      return go(Data_List_Types.Nil.value);
  })();
  var unsnoc = function (lst) {
      var go = function ($copy_v) {
          return function ($copy_acc) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, acc) {
                  if (v instanceof Data_List_Types.Nil) {
                      $tco_done = true;
                      return Data_Maybe.Nothing.value;
                  };
                  if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
                      $tco_done = true;
                      return new Data_Maybe.Just({
                          revInit: acc,
                          last: v.value0
                      });
                  };
                  if (v instanceof Data_List_Types.Cons) {
                      $tco_var_v = v.value1;
                      $copy_acc = new Data_List_Types.Cons(v.value0, acc);
                      return;
                  };
                  throw new Error("Failed pattern match at Data.List (line 270, column 3 - line 270, column 23): " + [ v.constructor.name, acc.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_v, $copy_acc);
              };
              return $tco_result;
          };
      };
      return Data_Functor.map(Data_Maybe.functorMaybe)(function (h) {
          return {
              init: reverse(h.revInit),
              last: h.last
          };
      })(go(lst)(Data_List_Types.Nil.value));
  };
  var zipWith = function (f) {
      return function (xs) {
          return function (ys) {
              var go = function ($copy_v) {
                  return function ($copy_v1) {
                      return function ($copy_acc) {
                          var $tco_var_v = $copy_v;
                          var $tco_var_v1 = $copy_v1;
                          var $tco_done = false;
                          var $tco_result;
                          function $tco_loop(v, v1, acc) {
                              if (v instanceof Data_List_Types.Nil) {
                                  $tco_done = true;
                                  return acc;
                              };
                              if (v1 instanceof Data_List_Types.Nil) {
                                  $tco_done = true;
                                  return acc;
                              };
                              if (v instanceof Data_List_Types.Cons && v1 instanceof Data_List_Types.Cons) {
                                  $tco_var_v = v.value1;
                                  $tco_var_v1 = v1.value1;
                                  $copy_acc = new Data_List_Types.Cons(f(v.value0)(v1.value0), acc);
                                  return;
                              };
                              throw new Error("Failed pattern match at Data.List (line 718, column 3 - line 718, column 21): " + [ v.constructor.name, v1.constructor.name, acc.constructor.name ]);
                          };
                          while (!$tco_done) {
                              $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_acc);
                          };
                          return $tco_result;
                      };
                  };
              };
              return reverse(go(xs)(ys)(Data_List_Types.Nil.value));
          };
      };
  };
  var range = function (start) {
      return function (end) {
          if (start === end) {
              return singleton(start);
          };
          if (Data_Boolean.otherwise) {
              var go = function ($copy_s) {
                  return function ($copy_e) {
                      return function ($copy_step) {
                          return function ($copy_rest) {
                              var $tco_var_s = $copy_s;
                              var $tco_var_e = $copy_e;
                              var $tco_var_step = $copy_step;
                              var $tco_done = false;
                              var $tco_result;
                              function $tco_loop(s, e, step, rest) {
                                  if (s === e) {
                                      $tco_done = true;
                                      return new Data_List_Types.Cons(s, rest);
                                  };
                                  if (Data_Boolean.otherwise) {
                                      $tco_var_s = s + step | 0;
                                      $tco_var_e = e;
                                      $tco_var_step = step;
                                      $copy_rest = new Data_List_Types.Cons(s, rest);
                                      return;
                                  };
                                  throw new Error("Failed pattern match at Data.List (line 148, column 3 - line 149, column 65): " + [ s.constructor.name, e.constructor.name, step.constructor.name, rest.constructor.name ]);
                              };
                              while (!$tco_done) {
                                  $tco_result = $tco_loop($tco_var_s, $tco_var_e, $tco_var_step, $copy_rest);
                              };
                              return $tco_result;
                          };
                      };
                  };
              };
              return go(end)(start)((function () {
                  var $220 = start > end;
                  if ($220) {
                      return 1;
                  };
                  return -1 | 0;
              })())(Data_List_Types.Nil.value);
          };
          throw new Error("Failed pattern match at Data.List (line 144, column 1 - line 144, column 32): " + [ start.constructor.name, end.constructor.name ]);
      };
  };
  var some = function (dictAlternative) {
      return function (dictLazy) {
          return function (v) {
              return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(Data_List_Types.Cons.create)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                  return many(dictAlternative)(dictLazy)(v);
              }));
          };
      };
  };
  var many = function (dictAlternative) {
      return function (dictLazy) {
          return function (v) {
              return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())(Data_List_Types.Nil.value));
          };
      };
  };
  var length = Data_Foldable.foldl(Data_List_Types.foldableList)(function (acc) {
      return function (v) {
          return acc + 1 | 0;
      };
  })(0);
  var last = function ($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
          if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
              $tco_done = true;
              return new Data_Maybe.Just(v.value0);
          };
          if (v instanceof Data_List_Types.Cons) {
              $copy_v = v.value1;
              return;
          };
          $tco_done = true;
          return Data_Maybe.Nothing.value;
      };
      while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
      };
      return $tco_result;
  };
  var init = function (lst) {
      return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
          return v.init;
      })(unsnoc(lst));
  };
  var head = function (v) {
      if (v instanceof Data_List_Types.Nil) {
          return Data_Maybe.Nothing.value;
      };
      if (v instanceof Data_List_Types.Cons) {
          return new Data_Maybe.Just(v.value0);
      };
      throw new Error("Failed pattern match at Data.List (line 230, column 1 - line 230, column 22): " + [ v.constructor.name ]);
  };
  var groupBy = function (v) {
      return function (v1) {
          if (v1 instanceof Data_List_Types.Nil) {
              return Data_List_Types.Nil.value;
          };
          if (v1 instanceof Data_List_Types.Cons) {
              var v2 = span(v(v1.value0))(v1.value1);
              return new Data_List_Types.Cons(new Data_NonEmpty.NonEmpty(v1.value0, v2.init), groupBy(v)(v2.rest));
          };
          throw new Error("Failed pattern match at Data.List (line 605, column 1 - line 605, column 80): " + [ v.constructor.name, v1.constructor.name ]);
      };
  };
  var findIndex = function (fn) {
      var go = function ($copy_v) {
          return function ($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                  if (v1 instanceof Data_List_Types.Cons) {
                      if (fn(v1.value0)) {
                          $tco_done = true;
                          return new Data_Maybe.Just(v);
                      };
                      if (Data_Boolean.otherwise) {
                          $tco_var_v = v + 1 | 0;
                          $copy_v1 = v1.value1;
                          return;
                      };
                  };
                  if (v1 instanceof Data_List_Types.Nil) {
                      $tco_done = true;
                      return Data_Maybe.Nothing.value;
                  };
                  throw new Error("Failed pattern match at Data.List (line 301, column 3 - line 301, column 35): " + [ v.constructor.name, v1.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_v, $copy_v1);
              };
              return $tco_result;
          };
      };
      return go(0);
  };
  var filter = function (p) {
      var go = function ($copy_acc) {
          return function ($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                  if (v instanceof Data_List_Types.Nil) {
                      $tco_done = true;
                      return reverse(acc);
                  };
                  if (v instanceof Data_List_Types.Cons) {
                      if (p(v.value0)) {
                          $tco_var_acc = new Data_List_Types.Cons(v.value0, acc);
                          $copy_v = v.value1;
                          return;
                      };
                      if (Data_Boolean.otherwise) {
                          $tco_var_acc = acc;
                          $copy_v = v.value1;
                          return;
                      };
                  };
                  throw new Error("Failed pattern match at Data.List (line 390, column 3 - line 390, column 27): " + [ acc.constructor.name, v.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_acc, $copy_v);
              };
              return $tco_result;
          };
      };
      return go(Data_List_Types.Nil.value);
  };                                                                              
  var concat = function (v) {
      return Control_Bind.bind(Data_List_Types.bindList)(v)(Control_Category.identity(Control_Category.categoryFn));
  };                                                                               
  var alterAt = function (v) {
      return function (v1) {
          return function (v2) {
              if (v === 0 && v2 instanceof Data_List_Types.Cons) {
                  return Data_Maybe.Just.create((function () {
                      var v3 = v1(v2.value0);
                      if (v3 instanceof Data_Maybe.Nothing) {
                          return v2.value1;
                      };
                      if (v3 instanceof Data_Maybe.Just) {
                          return new Data_List_Types.Cons(v3.value0, v2.value1);
                      };
                      throw new Error("Failed pattern match at Data.List (line 352, column 3 - line 354, column 23): " + [ v3.constructor.name ]);
                  })());
              };
              if (v2 instanceof Data_List_Types.Cons) {
                  return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                      return new Data_List_Types.Cons(v2.value0, v3);
                  })(alterAt(v - 1 | 0)(v1)(v2.value1));
              };
              return Data_Maybe.Nothing.value;
          };
      };
  };
  var modifyAt = function (n) {
      return function (f) {
          return alterAt(n)(function ($335) {
              return Data_Maybe.Just.create(f($335));
          });
      };
  };
  exports["toUnfoldable"] = toUnfoldable;
  exports["singleton"] = singleton;
  exports["range"] = range;
  exports["many"] = many;
  exports["length"] = length;
  exports["last"] = last;
  exports["init"] = init;
  exports["uncons"] = uncons;
  exports["findIndex"] = findIndex;
  exports["modifyAt"] = modifyAt;
  exports["concat"] = concat;
  exports["filter"] = filter;
  exports["sortBy"] = sortBy;
  exports["span"] = span;
  exports["groupBy"] = groupBy;
  exports["zipWith"] = zipWith;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.List.NonEmpty"] = $PS["Data.List.NonEmpty"] || {};
  var exports = $PS["Data.List.NonEmpty"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Tuple = $PS["Data.Tuple"];                        
  var zipWith = function (f) {
      return function (v) {
          return function (v1) {
              return new Data_NonEmpty.NonEmpty(f(v.value0)(v1.value0), Data_List.zipWith(f)(v.value1)(v1.value1));
          };
      };
  };
  var zip = zipWith(Data_Tuple.Tuple.create);
  var toList = function (v) {
      return new Data_List_Types.Cons(v.value0, v.value1);
  };
  var tail = function (v) {
      return v.value1;
  };
  var length = function (v) {
      return 1 + Data_List.length(v.value1) | 0;
  };
  var head = function (v) {
      return v.value0;
  };
  exports["toList"] = toList;
  exports["length"] = length;
  exports["head"] = head;
  exports["tail"] = tail;
  exports["zip"] = zip;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Map.Internal"] = $PS["Data.Map.Internal"] || {};
  var exports = $PS["Data.Map.Internal"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];                
  var Leaf = (function () {
      function Leaf() {

      };
      Leaf.value = new Leaf();
      return Leaf;
  })();
  var Two = (function () {
      function Two(value0, value1, value2, value3) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
          this.value3 = value3;
      };
      Two.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return function (value3) {
                      return new Two(value0, value1, value2, value3);
                  };
              };
          };
      };
      return Two;
  })();
  var Three = (function () {
      function Three(value0, value1, value2, value3, value4, value5, value6) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
          this.value3 = value3;
          this.value4 = value4;
          this.value5 = value5;
          this.value6 = value6;
      };
      Three.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return function (value3) {
                      return function (value4) {
                          return function (value5) {
                              return function (value6) {
                                  return new Three(value0, value1, value2, value3, value4, value5, value6);
                              };
                          };
                      };
                  };
              };
          };
      };
      return Three;
  })();
  var TwoLeft = (function () {
      function TwoLeft(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      TwoLeft.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new TwoLeft(value0, value1, value2);
              };
          };
      };
      return TwoLeft;
  })();
  var TwoRight = (function () {
      function TwoRight(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      TwoRight.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new TwoRight(value0, value1, value2);
              };
          };
      };
      return TwoRight;
  })();
  var ThreeLeft = (function () {
      function ThreeLeft(value0, value1, value2, value3, value4, value5) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
          this.value3 = value3;
          this.value4 = value4;
          this.value5 = value5;
      };
      ThreeLeft.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return function (value3) {
                      return function (value4) {
                          return function (value5) {
                              return new ThreeLeft(value0, value1, value2, value3, value4, value5);
                          };
                      };
                  };
              };
          };
      };
      return ThreeLeft;
  })();
  var ThreeMiddle = (function () {
      function ThreeMiddle(value0, value1, value2, value3, value4, value5) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
          this.value3 = value3;
          this.value4 = value4;
          this.value5 = value5;
      };
      ThreeMiddle.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return function (value3) {
                      return function (value4) {
                          return function (value5) {
                              return new ThreeMiddle(value0, value1, value2, value3, value4, value5);
                          };
                      };
                  };
              };
          };
      };
      return ThreeMiddle;
  })();
  var ThreeRight = (function () {
      function ThreeRight(value0, value1, value2, value3, value4, value5) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
          this.value3 = value3;
          this.value4 = value4;
          this.value5 = value5;
      };
      ThreeRight.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return function (value3) {
                      return function (value4) {
                          return function (value5) {
                              return new ThreeRight(value0, value1, value2, value3, value4, value5);
                          };
                      };
                  };
              };
          };
      };
      return ThreeRight;
  })();
  var KickUp = (function () {
      function KickUp(value0, value1, value2, value3) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
          this.value3 = value3;
      };
      KickUp.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return function (value3) {
                      return new KickUp(value0, value1, value2, value3);
                  };
              };
          };
      };
      return KickUp;
  })();
  var singleton = function (k) {
      return function (v) {
          return new Two(Leaf.value, k, v, Leaf.value);
      };
  };
  var toUnfoldable = function (dictUnfoldable) {
      return function (m) {
          var go = function ($copy_v) {
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v) {
                  if (v instanceof Data_List_Types.Nil) {
                      $tco_done = true;
                      return Data_Maybe.Nothing.value;
                  };
                  if (v instanceof Data_List_Types.Cons) {
                      if (v.value0 instanceof Leaf) {
                          $copy_v = v.value1;
                          return;
                      };
                      if (v.value0 instanceof Two && (v.value0.value0 instanceof Leaf && v.value0.value3 instanceof Leaf)) {
                          $tco_done = true;
                          return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), v.value1));
                      };
                      if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf) {
                          $tco_done = true;
                          return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), new Data_List_Types.Cons(v.value0.value3, v.value1)));
                      };
                      if (v.value0 instanceof Two) {
                          $copy_v = new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(singleton(v.value0.value1)(v.value0.value2), new Data_List_Types.Cons(v.value0.value3, v.value1)));
                          return;
                      };
                      if (v.value0 instanceof Three) {
                          $copy_v = new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(singleton(v.value0.value1)(v.value0.value2), new Data_List_Types.Cons(v.value0.value3, new Data_List_Types.Cons(singleton(v.value0.value4)(v.value0.value5), new Data_List_Types.Cons(v.value0.value6, v.value1)))));
                          return;
                      };
                      throw new Error("Failed pattern match at Data.Map.Internal (line 577, column 18 - line 586, column 71): " + [ v.value0.constructor.name ]);
                  };
                  throw new Error("Failed pattern match at Data.Map.Internal (line 576, column 3 - line 576, column 19): " + [ v.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($copy_v);
              };
              return $tco_result;
          };
          return Data_Unfoldable.unfoldr(dictUnfoldable)(go)(new Data_List_Types.Cons(m, Data_List_Types.Nil.value));
      };
  };
  var lookup = function (dictOrd) {
      return function (k) {
          var comp = Data_Ord.compare(dictOrd);
          var go = function ($copy_v) {
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v) {
                  if (v instanceof Leaf) {
                      $tco_done = true;
                      return Data_Maybe.Nothing.value;
                  };
                  if (v instanceof Two) {
                      var v2 = comp(k)(v.value1);
                      if (v2 instanceof Data_Ordering.EQ) {
                          $tco_done = true;
                          return new Data_Maybe.Just(v.value2);
                      };
                      if (v2 instanceof Data_Ordering.LT) {
                          $copy_v = v.value0;
                          return;
                      };
                      $copy_v = v.value3;
                      return;
                  };
                  if (v instanceof Three) {
                      var v3 = comp(k)(v.value1);
                      if (v3 instanceof Data_Ordering.EQ) {
                          $tco_done = true;
                          return new Data_Maybe.Just(v.value2);
                      };
                      var v4 = comp(k)(v.value4);
                      if (v4 instanceof Data_Ordering.EQ) {
                          $tco_done = true;
                          return new Data_Maybe.Just(v.value5);
                      };
                      if (v3 instanceof Data_Ordering.LT) {
                          $copy_v = v.value0;
                          return;
                      };
                      if (v4 instanceof Data_Ordering.GT) {
                          $copy_v = v.value6;
                          return;
                      };
                      $copy_v = v.value3;
                      return;
                  };
                  throw new Error("Failed pattern match at Data.Map.Internal (line 200, column 5 - line 200, column 22): " + [ v.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($copy_v);
              };
              return $tco_result;
          };
          return go;
      };
  };
  var keys = function (v) {
      if (v instanceof Leaf) {
          return Data_List_Types.Nil.value;
      };
      if (v instanceof Two) {
          return Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value1))(keys(v.value3)));
      };
      if (v instanceof Three) {
          return Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value1))(Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value3))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value4))(keys(v.value6)))));
      };
      throw new Error("Failed pattern match at Data.Map.Internal (line 606, column 1 - line 606, column 38): " + [ v.constructor.name ]);
  };
  var functorMap = new Data_Functor.Functor(function (v) {
      return function (v1) {
          if (v1 instanceof Leaf) {
              return Leaf.value;
          };
          if (v1 instanceof Two) {
              return new Two(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3));
          };
          if (v1 instanceof Three) {
              return new Three(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), Data_Functor.map(functorMap)(v)(v1.value6));
          };
          throw new Error("Failed pattern match at Data.Map.Internal (line 96, column 1 - line 99, column 110): " + [ v.constructor.name, v1.constructor.name ]);
      };
  });
  var fromZipper = function ($copy_dictOrd) {
      return function ($copy_v) {
          return function ($copy_tree) {
              var $tco_var_dictOrd = $copy_dictOrd;
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(dictOrd, v, tree) {
                  if (v instanceof Data_List_Types.Nil) {
                      $tco_done = true;
                      return tree;
                  };
                  if (v instanceof Data_List_Types.Cons) {
                      if (v.value0 instanceof TwoLeft) {
                          $tco_var_dictOrd = dictOrd;
                          $tco_var_v = v.value1;
                          $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
                          return;
                      };
                      if (v.value0 instanceof TwoRight) {
                          $tco_var_dictOrd = dictOrd;
                          $tco_var_v = v.value1;
                          $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
                          return;
                      };
                      if (v.value0 instanceof ThreeLeft) {
                          $tco_var_dictOrd = dictOrd;
                          $tco_var_v = v.value1;
                          $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
                          return;
                      };
                      if (v.value0 instanceof ThreeMiddle) {
                          $tco_var_dictOrd = dictOrd;
                          $tco_var_v = v.value1;
                          $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
                          return;
                      };
                      if (v.value0 instanceof ThreeRight) {
                          $tco_var_dictOrd = dictOrd;
                          $tco_var_v = v.value1;
                          $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
                          return;
                      };
                      throw new Error("Failed pattern match at Data.Map.Internal (line 418, column 3 - line 423, column 88): " + [ v.value0.constructor.name ]);
                  };
                  throw new Error("Failed pattern match at Data.Map.Internal (line 415, column 1 - line 415, column 80): " + [ v.constructor.name, tree.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
              };
              return $tco_result;
          };
      };
  };
  var insert = function (dictOrd) {
      return function (k) {
          return function (v) {
              var up = function ($copy_v1) {
                  return function ($copy_v2) {
                      var $tco_var_v1 = $copy_v1;
                      var $tco_done = false;
                      var $tco_result;
                      function $tco_loop(v1, v2) {
                          if (v1 instanceof Data_List_Types.Nil) {
                              $tco_done = true;
                              return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
                          };
                          if (v1 instanceof Data_List_Types.Cons) {
                              if (v1.value0 instanceof TwoLeft) {
                                  $tco_done = true;
                                  return fromZipper(dictOrd)(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                              };
                              if (v1.value0 instanceof TwoRight) {
                                  $tco_done = true;
                                  return fromZipper(dictOrd)(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                              };
                              if (v1.value0 instanceof ThreeLeft) {
                                  $tco_var_v1 = v1.value1;
                                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                                  return;
                              };
                              if (v1.value0 instanceof ThreeMiddle) {
                                  $tco_var_v1 = v1.value1;
                                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                                  return;
                              };
                              if (v1.value0 instanceof ThreeRight) {
                                  $tco_var_v1 = v1.value1;
                                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                                  return;
                              };
                              throw new Error("Failed pattern match at Data.Map.Internal (line 454, column 5 - line 459, column 108): " + [ v1.value0.constructor.name, v2.constructor.name ]);
                          };
                          throw new Error("Failed pattern match at Data.Map.Internal (line 451, column 3 - line 451, column 56): " + [ v1.constructor.name, v2.constructor.name ]);
                      };
                      while (!$tco_done) {
                          $tco_result = $tco_loop($tco_var_v1, $copy_v2);
                      };
                      return $tco_result;
                  };
              };
              var comp = Data_Ord.compare(dictOrd);
              var down = function ($copy_ctx) {
                  return function ($copy_v1) {
                      var $tco_var_ctx = $copy_ctx;
                      var $tco_done = false;
                      var $tco_result;
                      function $tco_loop(ctx, v1) {
                          if (v1 instanceof Leaf) {
                              $tco_done = true;
                              return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
                          };
                          if (v1 instanceof Two) {
                              var v2 = comp(k)(v1.value1);
                              if (v2 instanceof Data_Ordering.EQ) {
                                  $tco_done = true;
                                  return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                              };
                              if (v2 instanceof Data_Ordering.LT) {
                                  $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                                  $copy_v1 = v1.value0;
                                  return;
                              };
                              $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                              $copy_v1 = v1.value3;
                              return;
                          };
                          if (v1 instanceof Three) {
                              var v3 = comp(k)(v1.value1);
                              if (v3 instanceof Data_Ordering.EQ) {
                                  $tco_done = true;
                                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                              };
                              var v4 = comp(k)(v1.value4);
                              if (v4 instanceof Data_Ordering.EQ) {
                                  $tco_done = true;
                                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                              };
                              if (v3 instanceof Data_Ordering.LT) {
                                  $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                                  $copy_v1 = v1.value0;
                                  return;
                              };
                              if (v3 instanceof Data_Ordering.GT && v4 instanceof Data_Ordering.LT) {
                                  $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                                  $copy_v1 = v1.value3;
                                  return;
                              };
                              $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                              $copy_v1 = v1.value6;
                              return;
                          };
                          throw new Error("Failed pattern match at Data.Map.Internal (line 434, column 3 - line 434, column 55): " + [ ctx.constructor.name, v1.constructor.name ]);
                      };
                      while (!$tco_done) {
                          $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
                      };
                      return $tco_result;
                  };
              };
              return down(Data_List_Types.Nil.value);
          };
      };
  };
  var pop = function (dictOrd) {
      return function (k) {
          var up = function ($copy_ctxs) {
              return function ($copy_tree) {
                  var $tco_var_ctxs = $copy_ctxs;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(ctxs, tree) {
                      if (ctxs instanceof Data_List_Types.Nil) {
                          $tco_done = true;
                          return tree;
                      };
                      if (ctxs instanceof Data_List_Types.Cons) {
                          if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
                          };
                          if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
                          };
                          if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                              $tco_var_ctxs = ctxs.value1;
                              $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                              return;
                          };
                          if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                              $tco_var_ctxs = ctxs.value1;
                              $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                              return;
                          };
                          if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
                          };
                          if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
                          };
                          if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
                          };
                          if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
                          };
                          if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
                          };
                          if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                          };
                          if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                          };
                          if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
                          };
                          if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
                          };
                          if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                          };
                          if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                          };
                          if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
                          };
                          if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                              $tco_done = true;
                              return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
                          };
                          throw new Error("Failed pattern match at Data.Map.Internal (line 511, column 9 - line 528, column 136): " + [ ctxs.value0.constructor.name, tree.constructor.name ]);
                      };
                      throw new Error("Failed pattern match at Data.Map.Internal (line 508, column 5 - line 528, column 136): " + [ ctxs.constructor.name ]);
                  };
                  while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
                  };
                  return $tco_result;
              };
          };
          var removeMaxNode = function ($copy_ctx) {
              return function ($copy_m) {
                  var $tco_var_ctx = $copy_ctx;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(ctx, m) {
                      if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
                          $tco_done = true;
                          return up(ctx)(Leaf.value);
                      };
                      if (m instanceof Two) {
                          $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
                          $copy_m = m.value3;
                          return;
                      };
                      if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
                          $tco_done = true;
                          return up(new Data_List_Types.Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
                      };
                      if (m instanceof Three) {
                          $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
                          $copy_m = m.value6;
                          return;
                      };
                      throw new Error("Failed pattern match at Data.Map.Internal (line 540, column 5 - line 544, column 107): " + [ m.constructor.name ]);
                  };
                  while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_ctx, $copy_m);
                  };
                  return $tco_result;
              };
          };
          var maxNode = function ($copy_m) {
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(m) {
                  if (m instanceof Two && m.value3 instanceof Leaf) {
                      $tco_done = true;
                      return {
                          key: m.value1,
                          value: m.value2
                      };
                  };
                  if (m instanceof Two) {
                      $copy_m = m.value3;
                      return;
                  };
                  if (m instanceof Three && m.value6 instanceof Leaf) {
                      $tco_done = true;
                      return {
                          key: m.value4,
                          value: m.value5
                      };
                  };
                  if (m instanceof Three) {
                      $copy_m = m.value6;
                      return;
                  };
                  throw new Error("Failed pattern match at Data.Map.Internal (line 531, column 33 - line 535, column 45): " + [ m.constructor.name ]);
              };
              while (!$tco_done) {
                  $tco_result = $tco_loop($copy_m);
              };
              return $tco_result;
          };
          var comp = Data_Ord.compare(dictOrd);
          var down = function ($copy_ctx) {
              return function ($copy_m) {
                  var $tco_var_ctx = $copy_ctx;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(ctx, m) {
                      if (m instanceof Leaf) {
                          $tco_done = true;
                          return Data_Maybe.Nothing.value;
                      };
                      if (m instanceof Two) {
                          var v = comp(k)(m.value1);
                          if (m.value3 instanceof Leaf && v instanceof Data_Ordering.EQ) {
                              $tco_done = true;
                              return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, up(ctx)(Leaf.value)));
                          };
                          if (v instanceof Data_Ordering.EQ) {
                              var max = maxNode(m.value0);
                              $tco_done = true;
                              return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new TwoLeft(max.key, max.value, m.value3), ctx))(m.value0)));
                          };
                          if (v instanceof Data_Ordering.LT) {
                              $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                              $copy_m = m.value0;
                              return;
                          };
                          $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
                          $copy_m = m.value3;
                          return;
                      };
                      if (m instanceof Three) {
                          var leaves = (function () {
                              if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                                  return true;
                              };
                              return false;
                          })();
                          var v = comp(k)(m.value4);
                          var v3 = comp(k)(m.value1);
                          if (leaves && v3 instanceof Data_Ordering.EQ) {
                              $tco_done = true;
                              return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
                          };
                          if (leaves && v instanceof Data_Ordering.EQ) {
                              $tco_done = true;
                              return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
                          };
                          if (v3 instanceof Data_Ordering.EQ) {
                              var max = maxNode(m.value0);
                              $tco_done = true;
                              return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new ThreeLeft(max.key, max.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
                          };
                          if (v instanceof Data_Ordering.EQ) {
                              var max = maxNode(m.value3);
                              $tco_done = true;
                              return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, removeMaxNode(new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max.key, max.value, m.value6), ctx))(m.value3)));
                          };
                          if (v3 instanceof Data_Ordering.LT) {
                              $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                              $copy_m = m.value0;
                              return;
                          };
                          if (v3 instanceof Data_Ordering.GT && v instanceof Data_Ordering.LT) {
                              $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                              $copy_m = m.value3;
                              return;
                          };
                          $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
                          $copy_m = m.value6;
                          return;
                      };
                      throw new Error("Failed pattern match at Data.Map.Internal (line 481, column 34 - line 504, column 80): " + [ m.constructor.name ]);
                  };
                  while (!$tco_done) {
                      $tco_result = $tco_loop($tco_var_ctx, $copy_m);
                  };
                  return $tco_result;
              };
          };
          return down(Data_List_Types.Nil.value);
      };
  };
  var empty = Leaf.value;
  var fromFoldable = function (dictOrd) {
      return function (dictFoldable) {
          return Data_Foldable.foldl(dictFoldable)(function (m) {
              return function (v) {
                  return insert(dictOrd)(v.value0)(v.value1)(m);
              };
          })(empty);
      };
  };
  var $$delete = function (dictOrd) {
      return function (k) {
          return function (m) {
              return Data_Maybe.maybe(m)(Data_Tuple.snd)(pop(dictOrd)(k)(m));
          };
      };
  };
  exports["insert"] = insert;
  exports["lookup"] = lookup;
  exports["fromFoldable"] = fromFoldable;
  exports["toUnfoldable"] = toUnfoldable;
  exports["delete"] = $$delete;
  exports["keys"] = keys;
  exports["functorMap"] = functorMap;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Map"] = $PS["Data.Map"] || {};
  var exports = $PS["Data.Map"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];                
  var keys = (function () {
      var $0 = Data_Functor["void"](Data_Map_Internal.functorMap);
      return function ($1) {
          return $0($1);
      };
  })();
  exports["keys"] = keys;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Pair"] = $PS["Data.Pair"] || {};
  var exports = $PS["Data.Pair"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];                          
  var Pair = (function () {
      function Pair(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Pair.create = function (value0) {
          return function (value1) {
              return new Pair(value0, value1);
          };
      };
      return Pair;
  })();
  var semigroupPair = function (dictSemigroup) {
      return new Data_Semigroup.Semigroup(function (v) {
          return function (v1) {
              return new Pair(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), Data_Semigroup.append(dictSemigroup)(v.value1)(v1.value1));
          };
      });
  };
  var monoidPair = function (dictMonoid) {
      return new Data_Monoid.Monoid(function () {
          return semigroupPair(dictMonoid.Semigroup0());
      }, new Pair(Data_Monoid.mempty(dictMonoid), Data_Monoid.mempty(dictMonoid)));
  };
  var functorPair = new Data_Functor.Functor(function (f) {
      return function (v) {
          return new Pair(f(v.value0), f(v.value1));
      };
  });
  var eqPair = function (dictEq) {
      return new Data_Eq.Eq(function (x) {
          return function (y) {
              return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq)(x.value1)(y.value1);
          };
      });
  };
  exports["Pair"] = Pair;
  exports["eqPair"] = eqPair;
  exports["functorPair"] = functorPair;
  exports["monoidPair"] = monoidPair;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units"] = $PS["Data.Units"] || {};
  var exports = $PS["Data.Units"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Decimal = $PS["Data.Decimal"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List = $PS["Data.List"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Monoid_Additive = $PS["Data.Monoid.Additive"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Pair = $PS["Data.Pair"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Show = $PS["Data.Show"];
  var Data_Tuple = $PS["Data.Tuple"];                
  var Decimal = (function () {
      function Decimal() {

      };
      Decimal.value = new Decimal();
      return Decimal;
  })();
  var Binary = (function () {
      function Binary() {

      };
      Binary.value = new Binary();
      return Binary;
  })();
  var Prefix = (function () {
      function Prefix(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Prefix.create = function (value0) {
          return function (value1) {
              return new Prefix(value0, value1);
          };
      };
      return Prefix;
  })();
  var Standard = (function () {
      function Standard() {

      };
      Standard.value = new Standard();
      return Standard;
  })();
  var NonStandard = (function () {
      function NonStandard(value0) {
          this.value0 = value0;
      };
      NonStandard.create = function (value0) {
          return new NonStandard(value0);
      };
      return NonStandard;
  })();
  var DerivedUnit = (function () {
      function DerivedUnit(value0) {
          this.value0 = value0;
      };
      DerivedUnit.create = function (value0) {
          return new DerivedUnit(value0);
      };
      return DerivedUnit;
  })();
  var unity$prime = {
      "short": "unity",
      "long": "unity",
      unitType: Standard.value
  };
  var unity = new DerivedUnit(Data_List_Types.Nil.value);
  var split = function (v) {
      return function (v1) {
          if (v1 instanceof Data_List_Types.Nil) {
              return {
                  yes: Data_List_Types.Nil.value,
                  no: Data_List_Types.Nil.value
              };
          };
          if (v1 instanceof Data_List_Types.Cons) {
              var res = split(v)(v1.value1);
              var $72 = v(v1.value0);
              if ($72) {
                  return {
                      yes: new Data_List_Types.Cons(v1.value0, res.yes),
                      no: res.no
                  };
              };
              return {
                  yes: res.yes,
                  no: new Data_List_Types.Cons(v1.value0, res.no)
              };
          };
          throw new Error("Failed pattern match at Data.Units (line 199, column 1 - line 199, column 69): " + [ v.constructor.name, v1.constructor.name ]);
      };
  };
  var sortBy$prime = function (f) {
      return function (v) {
          var sorted = Data_List.sortBy(f)(new Data_List_Types.Cons(v.value0, v.value1));
          var nel = (function () {
              var v1 = Data_List.uncons(sorted);
              if (v1 instanceof Data_Maybe.Just) {
                  return new Data_NonEmpty.NonEmpty(v1.value0.head, v1.value0.tail);
              };
              if (v1 instanceof Data_Maybe.Nothing) {
                  return new Data_NonEmpty.NonEmpty(v.value0, v.value1);
              };
              throw new Error("Failed pattern match at Data.Units (line 222, column 11 - line 224, column 45): " + [ v1.constructor.name ]);
          })();
          return nel;
      };
  };
  var shortName = function (v) {
      return v["short"];
  };
  var runDerivedUnit = function (v) {
      return v.value0;
  };
  var prettyExponent = function (v) {
      if (v === -5.0) {
          return "\u207b\u2075";
      };
      if (v === -4.0) {
          return "\u207b\u2074";
      };
      if (v === -3.0) {
          return "\u207b\xb3";
      };
      if (v === -2.0) {
          return "\u207b\xb2";
      };
      if (v === -1.0) {
          return "\u207b\xb9";
      };
      if (v === 1.0) {
          return "";
      };
      if (v === 2.0) {
          return "\xb2";
      };
      if (v === 3.0) {
          return "\xb3";
      };
      if (v === 4.0) {
          return "\u2074";
      };
      if (v === 5.0) {
          return "\u2075";
      };
      return "^(" + (Data_Show.show(Data_Show.showNumber)(v) + ")");
  };
  var prefixName = function (v) {
      if (v.value0 instanceof Decimal) {
          var pn = function (v1) {
              if (v1 === -18.0) {
                  return new Data_Maybe.Just("a");
              };
              if (v1 === -12.0) {
                  return new Data_Maybe.Just("p");
              };
              if (v1 === -15.0) {
                  return new Data_Maybe.Just("f");
              };
              if (v1 === -9.0) {
                  return new Data_Maybe.Just("n");
              };
              if (v1 === -6.0) {
                  return new Data_Maybe.Just("\xb5");
              };
              if (v1 === -3.0) {
                  return new Data_Maybe.Just("m");
              };
              if (v1 === -2.0) {
                  return new Data_Maybe.Just("c");
              };
              if (v1 === -1.0) {
                  return new Data_Maybe.Just("d");
              };
              if (v1 === 0.0) {
                  return new Data_Maybe.Just("");
              };
              if (v1 === 2.0) {
                  return new Data_Maybe.Just("h");
              };
              if (v1 === 3.0) {
                  return new Data_Maybe.Just("k");
              };
              if (v1 === 6.0) {
                  return new Data_Maybe.Just("M");
              };
              if (v1 === 9.0) {
                  return new Data_Maybe.Just("G");
              };
              if (v1 === 12.0) {
                  return new Data_Maybe.Just("T");
              };
              if (v1 === 15.0) {
                  return new Data_Maybe.Just("P");
              };
              if (v1 === 18.0) {
                  return new Data_Maybe.Just("E");
              };
              return Data_Maybe.Nothing.value;
          };
          return pn(Data_Decimal.toNumber(v.value1));
      };
      if (v.value0 instanceof Binary) {
          var pn = function (v1) {
              if (v1 === 0.0) {
                  return new Data_Maybe.Just("");
              };
              if (v1 === 10.0) {
                  return new Data_Maybe.Just("Ki");
              };
              if (v1 === 20.0) {
                  return new Data_Maybe.Just("Mi");
              };
              if (v1 === 30.0) {
                  return new Data_Maybe.Just("Gi");
              };
              if (v1 === 40.0) {
                  return new Data_Maybe.Just("Ti");
              };
              if (v1 === 50.0) {
                  return new Data_Maybe.Just("Pi");
              };
              if (v1 === 60.0) {
                  return new Data_Maybe.Just("Ei");
              };
              if (v1 === 70.0) {
                  return new Data_Maybe.Just("Zi");
              };
              if (v1 === 80.0) {
                  return new Data_Maybe.Just("Yi");
              };
              return Data_Maybe.Nothing.value;
          };
          return pn(Data_Decimal.toNumber(v.value1));
      };
      throw new Error("Failed pattern match at Data.Units (line 415, column 1 - line 415, column 35): " + [ v.constructor.name ]);
  };
  var toString = function (v) {
      var usSorted = Data_List.sortBy(Data_Ord.comparing(Data_Ord.ordNumber)(function (rec) {
          return -rec.exponent;
      }))(v.value0);
      var toNum = function (v1) {
          if (v1 instanceof Decimal) {
              return 10;
          };
          if (v1 instanceof Binary) {
              return 2;
          };
          throw new Error("Failed pattern match at Data.Units (line 466, column 5 - line 466, column 23): " + [ v1.constructor.name ]);
      };
      var splitted = Data_List.span(function (rec) {
          return rec.exponent >= 0.0;
      })(usSorted);
      var reverseExp = function (rec) {
          return {
              exponent: -rec.exponent,
              baseUnit: rec.baseUnit,
              prefix: rec.prefix
          };
      };
      var prefixName$prime = function (v1) {
          return Data_Maybe.fromMaybe(Data_Show.show(Data_Show.showInt)(toNum(v1.value0)) + ("^" + (Data_Show.show(Data_Decimal.showDecimal)(v1.value1) + "\xb7")))(prefixName(v1));
      };
      var withExp = function (v1) {
          return prefixName$prime(v1.prefix) + (shortName(v1.baseUnit) + prettyExponent(v1.exponent));
      };
      var positiveUsStr = Data_Foldable.intercalate(Data_List_Types.foldableList)(Data_Monoid.monoidString)("\xb7")(Data_Functor.map(Data_List_Types.functorList)(withExp)(splitted.init));
      var negativeUs = Data_List.sortBy(Data_Ord.comparing(Data_Ord.ordNumber)(function (v1) {
          return v1.exponent;
      }))(splitted.rest);
      var negativeUsStr = Data_Foldable.intercalate(Data_List_Types.foldableList)(Data_Monoid.monoidString)("\xb7")(Data_Functor.map(Data_List_Types.functorList)(withExp)(negativeUs));
      var negativeUsStr$prime = Data_Foldable.intercalate(Data_List_Types.foldableList)(Data_Monoid.monoidString)("\xb7")(Data_Functor.map(Data_List_Types.functorList)(function ($219) {
          return withExp(reverseExp($219));
      })(negativeUs));
      var unitString = (function () {
          if (splitted.init instanceof Data_List_Types.Nil) {
              return negativeUsStr;
          };
          if (negativeUs instanceof Data_List_Types.Nil) {
              return positiveUsStr;
          };
          if (negativeUs instanceof Data_List_Types.Cons && negativeUs.value1 instanceof Data_List_Types.Nil) {
              return positiveUsStr + ("/" + negativeUsStr$prime);
          };
          return positiveUsStr + ("/(" + (negativeUsStr$prime + ")"));
      })();
      return unitString;
  };
  var power = function (u) {
      return function (n) {
          var update = function (rec) {
              return {
                  exponent: rec.exponent * n,
                  baseUnit: rec.baseUnit,
                  prefix: rec.prefix
              };
          };
          return DerivedUnit.create(Data_Functor.map(Data_List_Types.functorList)(update)(runDerivedUnit(u)));
      };
  };
  var noPrefix = new Prefix(Decimal.value, Data_Semiring.zero(Data_Decimal.semiringDecimal));
  var removePrefix = function (v) {
      return DerivedUnit.create(Data_Functor.map(Data_List_Types.functorList)(function (v1) {
          return {
              prefix: noPrefix,
              baseUnit: v1.baseUnit,
              exponent: v1.exponent
          };
      })(v.value0));
  };
  var longName = function (v) {
      return v["long"];
  }; 
  var isStandardUnit = function (v) {
      if (v.unitType instanceof Standard) {
          return true;
      };
      return false;
  };
  var groupBy$prime = function (v) {
      return function (v1) {
          if (v1 instanceof Data_List_Types.Nil) {
              return Data_List_Types.Nil.value;
          };
          if (v1 instanceof Data_List_Types.Cons) {
              var v2 = split(v(v1.value0))(v1.value1);
              return new Data_List_Types.Cons(new Data_NonEmpty.NonEmpty(v1.value0, v2.yes), groupBy$prime(v)(v2.no));
          };
          throw new Error("Failed pattern match at Data.Units (line 212, column 1 - line 212, column 67): " + [ v.constructor.name, v1.constructor.name ]);
      };
  };
  var fromBaseUnit = function ($220) {
      return DerivedUnit.create(Data_List.singleton((function (v) {
          return {
              prefix: noPrefix,
              baseUnit: v,
              exponent: 1.0
          };
      })($220)));
  };
  var makeNonStandard = function ($$long) {
      return function ($$short) {
          return function (factor) {
              return function (standardUnit) {
                  return fromBaseUnit({
                      "short": $$short,
                      "long": $$long,
                      unitType: new NonStandard({
                          standardUnit: standardUnit,
                          factor: Data_Decimal.fromNumber(factor)
                      })
                  });
              };
          };
      };
  };
  var makeStandard = function ($$long) {
      return function ($$short) {
          return fromBaseUnit({
              "short": $$short,
              "long": $$long,
              unitType: Standard.value
          });
      };
  };
  var eqPrefixBase = new Data_Eq.Eq(function (x) {
      return function (y) {
          if (x instanceof Decimal && y instanceof Decimal) {
              return true;
          };
          if (x instanceof Binary && y instanceof Binary) {
              return true;
          };
          return false;
      };
  });
  var withPrefix = function (base) {
      return function (p) {
          return function (v) {
              if (v.value0 instanceof Data_List_Types.Nil) {
                  return DerivedUnit.create(Data_List.singleton({
                      prefix: new Prefix(base, Data_Decimal.fromNumber(p)),
                      baseUnit: unity$prime,
                      exponent: 1.0
                  }));
              };
              var isPlaceholder = function (v1) {
                  return v1.exponent === 1.0 && (Data_Eq.eq(eqPrefixBase)(base)(v1.prefix.value0) || Data_Eq.eq(Data_Decimal.eqDecimal)(v1.prefix.value1)(Data_Semiring.zero(Data_Decimal.semiringDecimal)));
              };
              var addPrefixExp = function (du) {
                  return {
                      prefix: new Prefix(base, Data_Semiring.add(Data_Decimal.semiringDecimal)(Data_Decimal.fromNumber(p))(du.prefix.value1)),
                      baseUnit: du.baseUnit,
                      exponent: du.exponent
                  };
              };
              return DerivedUnit.create((function () {
                  var v1 = Data_List.findIndex(isPlaceholder)(v.value0);
                  if (v1 instanceof Data_Maybe.Just) {
                      return Data_Maybe.fromMaybe(v.value0)(Data_List.modifyAt(v1.value0)(addPrefixExp)(v.value0));
                  };
                  if (v1 instanceof Data_Maybe.Nothing) {
                      return new Data_List_Types.Cons({
                          prefix: new Prefix(base, Data_Decimal.fromNumber(p)),
                          baseUnit: unity$prime,
                          exponent: 1.0
                      }, v.value0);
                  };
                  throw new Error("Failed pattern match at Data.Units (line 172, column 3 - line 175, column 91): " + [ v1.constructor.name ]);
              })());
          };
      };
  };
  var eqPrefix = new Data_Eq.Eq(function (v) {
      return function (v1) {
          return Data_Eq.eq(Data_Decimal.eqDecimal)(v.value1)(Data_Semiring.zero(Data_Decimal.semiringDecimal)) && Data_Eq.eq(Data_Decimal.eqDecimal)(v1.value1)(Data_Semiring.zero(Data_Decimal.semiringDecimal)) || Data_Eq.eq(eqPrefixBase)(v.value0)(v1.value0) && Data_Eq.eq(Data_Decimal.eqDecimal)(v.value1)(v1.value1);
      };
  });
  var simplify = function (v) {
      var merge = function (units) {
          return {
              prefix: (Data_List_NonEmpty.head(units)).prefix,
              baseUnit: (Data_List_NonEmpty.head(units)).baseUnit,
              exponent: Data_Foldable.sum(Data_List_Types.foldableNonEmptyList)(Data_Semiring.semiringNumber)(Data_Functor.map(Data_List_Types.functorNonEmptyList)(function (v1) {
                  return v1.exponent;
              })(units))
          };
      };
      var go = (function () {
          var $221 = Data_List.filter(function (x) {
              return !(x.exponent === 0.0);
          });
          var $222 = Data_Functor.map(Data_List_Types.functorList)(merge);
          var $223 = groupBy$prime(function (u1) {
              return function (u2) {
                  return Data_Eq.eq(eqBaseUnit)(u1.baseUnit)(u2.baseUnit) && Data_Eq.eq(eqPrefix)(u1.prefix)(u2.prefix);
              };
          });
          var $224 = Data_Functor.map(Data_List_Types.functorList)(Data_List_NonEmpty.toList);
          var $225 = groupBy$prime(function (u1) {
              return function (u2) {
                  return Data_Eq.eq(eqBaseUnit)(u1.baseUnit)(u2.baseUnit);
              };
          });
          return function ($226) {
              return $221($222($223(Data_List.concat($224($225($226))))));
          };
      })();
      return new DerivedUnit(go(v.value0));
  };
  var eqUnitType = new Data_Eq.Eq(function (v) {
      return function (v1) {
          if (v instanceof Standard && v1 instanceof Standard) {
              return true;
          };
          if (v instanceof NonStandard && v1 instanceof NonStandard) {
              return Data_Eq.eq(eqDerivedUnit)(v.value0.standardUnit)(v1.value0.standardUnit) && Data_Eq.eq(Data_Decimal.eqDecimal)(v.value0.factor)(v1.value0.factor);
          };
          return false;
      };
  });
  var eqDerivedUnit = new Data_Eq.Eq(function (u1) {
      return function (u2) {
          var removeUnity = Data_List.filter(function (u) {
              return longName(u.baseUnit) !== "unity";
          });
          var prepare = (function () {
              var $227 = Data_List.sortBy(Data_Ord.comparing(Data_Ord.ordString)(function ($229) {
                  return shortName((function (v) {
                      return v.baseUnit;
                  })($229));
              }));
              return function ($228) {
                  return $227(runDerivedUnit(simplify($228)));
              };
          })();
          var list2 = prepare(u2);
          var list2$prime = removeUnity(list2);
          var list1 = prepare(u1);
          var list1$prime = removeUnity(list1);
          var globalPrefix = function (us) {
              var toPair = function (v) {
                  if (v.value0 instanceof Decimal) {
                      return new Data_Pair.Pair(v.value1, Data_Semiring.zero(Data_Decimal.semiringDecimal));
                  };
                  if (v.value0 instanceof Binary) {
                      return new Data_Pair.Pair(Data_Semiring.zero(Data_Decimal.semiringDecimal), v.value1);
                  };
                  throw new Error("Failed pattern match at Data.Units (line 335, column 11 - line 335, column 50): " + [ v.constructor.name ]);
              };
              var prefixPair = function (v) {
                  return Data_Functor.map(Data_Pair.functorPair)(function (v1) {
                      return Data_Semiring.mul(Data_Decimal.semiringDecimal)(v1)(Data_Decimal.fromNumber(v.exponent));
                  })(toPair(v.prefix));
              };
              return Data_Functor.map(Data_Pair.functorPair)(Data_Newtype.un(Data_Newtype.newtypeAdditive)(Data_Monoid_Additive.Additive))(Data_Foldable.fold(Data_List_Types.foldableList)(Data_Pair.monoidPair(Data_Monoid_Additive.monoidAdditive(Data_Decimal.semiringDecimal)))(Data_Functor.map(Data_List_Types.functorList)((function () {
                  var $230 = Data_Functor.map(Data_Pair.functorPair)(Data_Monoid_Additive.Additive);
                  return function ($231) {
                      return $230(prefixPair($231));
                  };
              })())(us)));
          };
          return Data_Eq.eq(Data_List_Types.eqList(eqBaseUnit))(Data_Functor.map(Data_List_Types.functorList)(function (v) {
              return v.baseUnit;
          })(list1$prime))(Data_Functor.map(Data_List_Types.functorList)(function (v) {
              return v.baseUnit;
          })(list2$prime)) && (Data_Eq.eq(Data_List_Types.eqList(Data_Eq.eqNumber))(Data_Functor.map(Data_List_Types.functorList)(function (v) {
              return v.exponent;
          })(list1$prime))(Data_Functor.map(Data_List_Types.functorList)(function (v) {
              return v.exponent;
          })(list2$prime)) && Data_Eq.eq(Data_Pair.eqPair(Data_Decimal.eqDecimal))(globalPrefix(list1))(globalPrefix(list2)));
      };
  });
  var eqBaseUnit = new Data_Eq.Eq(function (v) {
      return function (v1) {
          return v["long"] === v1["long"] && (v["short"] === v1["short"] && Data_Eq.eq(eqUnitType)(v.unitType)(v1.unitType));
      };
  });
  var semigroupDerivedUnit = new Data_Semigroup.Semigroup(function (v) {
      return function (v1) {
          return simplify(new DerivedUnit(Data_Semigroup.append(Data_List_Types.semigroupList)(v.value0)(v1.value0)));
      };
  });
  var monoidDerivedUnit = new Data_Monoid.Monoid(function () {
      return semigroupDerivedUnit;
  }, unity);
  var divideUnits = function (du1) {
      return function (du2) {
          return Data_Semigroup.append(semigroupDerivedUnit)(du1)(power(du2)(-1.0));
      };
  };
  var decimalPrefix = withPrefix(Decimal.value);
  var exa = decimalPrefix(18.0);
  var femto = decimalPrefix(-15.0);
  var giga = decimalPrefix(9.0);
  var hecto = decimalPrefix(2.0);
  var kilo = decimalPrefix(3.0);
  var mega = decimalPrefix(6.0);
  var micro = decimalPrefix(-6.0);
  var milli = decimalPrefix(-3.0);
  var nano = decimalPrefix(-9.0);
  var peta = decimalPrefix(15.0);
  var pico = decimalPrefix(-12.0);
  var tera = decimalPrefix(12.0);
  var deci = decimalPrefix(-1.0);
  var conversionFactor = function (v) {
      if (v.unitType instanceof Standard) {
          return Data_Semiring.one(Data_Decimal.semiringDecimal);
      };
      if (v.unitType instanceof NonStandard) {
          return v.unitType.value0.factor;
      };
      throw new Error("Failed pattern match at Data.Units (line 123, column 3 - line 125, column 50): " + [ v.unitType.constructor.name ]);
  };
  var centi = decimalPrefix(-2.0);
  var binaryPrefix = withPrefix(Binary.value);
  var exbi = binaryPrefix(60.0);
  var gibi = binaryPrefix(30.0);
  var kibi = binaryPrefix(10.0);
  var mebi = binaryPrefix(20.0);
  var pebi = binaryPrefix(50.0);
  var tebi = binaryPrefix(40.0);
  var yobi = binaryPrefix(80.0);
  var zebi = binaryPrefix(70.0);
  var baseToStandard = function (v) {
      if (v.unitType instanceof Standard) {
          return fromBaseUnit(v);
      };
      if (v.unitType instanceof NonStandard) {
          return v.unitType.value0.standardUnit;
      };
      throw new Error("Failed pattern match at Data.Units (line 117, column 3 - line 119, column 56): " + [ v.unitType.constructor.name ]);
  };
  var splitByDimension = function (v) {
      var standardUnit = function ($232) {
          return baseToStandard((function (v1) {
              return v1.baseUnit;
          })($232));
      };
      var removeExponent = function (v1) {
          if (v1.value0 instanceof Data_List_Types.Cons && v1.value0.value1 instanceof Data_List_Types.Nil) {
              return DerivedUnit.create(Data_List.singleton({
                  exponent: 1.0,
                  baseUnit: v1.value0.value0.baseUnit,
                  prefix: v1.value0.value0.prefix
              }));
          };
          return v1;
      };
      var standardUnitWithoutExponent = function ($233) {
          return removeExponent(standardUnit($233));
      };
      var heuristic = function (b1) {
          return function (b2) {
              return Data_Semigroup.append(Data_Ordering.semigroupOrdering)(Data_Ord.compare(Data_Ord.ordBoolean)(isStandardUnit(b1.baseUnit))(isStandardUnit(b2.baseUnit)))(Data_Ord.compare(Data_Ord.ordNumber)(b2.exponent)(b1.exponent));
          };
      };
      var exponentWRT = function (base) {
          return function (u) {
              var removedExponent = function (u$prime) {
                  var v1 = standardUnit(u$prime);
                  if (v1.value0 instanceof Data_List_Types.Cons && v1.value0.value1 instanceof Data_List_Types.Nil) {
                      return v1.value0.value0.exponent;
                  };
                  return 1.0;
              };
              return (u.exponent * removedExponent(u)) / removedExponent(base);
          };
      };
      var reduce = function (us$prime) {
          var us = sortBy$prime(Data_Function.flip(heuristic))(us$prime);
          var first = Data_List_NonEmpty.head(us);
          var exp = Data_Foldable.sum(Data_List_Types.foldableNonEmptyList)(Data_Semiring.semiringNumber)(Data_Functor.map(Data_List_Types.functorNonEmptyList)(exponentWRT(first))(us));
          var convertTo = DerivedUnit.create(Data_List.singleton({
              exponent: exp,
              baseUnit: first.baseUnit,
              prefix: first.prefix
          }));
          return new Data_Tuple.Tuple(convertTo, DerivedUnit.create(Data_List_NonEmpty.toList(us)));
      };
      var transform = (function () {
          var $234 = Data_Functor.map(Data_List_Types.functorList)(reduce);
          var $235 = groupBy$prime(Data_Function.on(Data_Eq.eq(eqDerivedUnit))(standardUnitWithoutExponent));
          return function ($236) {
              return $234($235($236));
          };
      })();
      return transform(v.value0);
  };
  var toStandardUnit = function (v) {
      var convert = function (v1) {
          var toNum = function (v2) {
              if (v2 instanceof Decimal) {
                  return Data_Decimal.fromNumber(10.0);
              };
              if (v2 instanceof Binary) {
                  return Data_Decimal.fromNumber(2.0);
              };
              throw new Error("Failed pattern match at Data.Units (line 408, column 11 - line 408, column 42): " + [ v2.constructor.name ]);
          };
          var standardUnit = baseToStandard(v1.baseUnit);
          var factor = conversionFactor(v1.baseUnit);
          var exponent$prime = Data_Decimal.fromNumber(v1.exponent);
          return new Data_Tuple.Tuple(power(standardUnit)(v1.exponent), Data_Decimal.pow(Data_Semiring.mul(Data_Decimal.semiringDecimal)(Data_Decimal.pow(toNum(v1.prefix.value0))(v1.prefix.value1))(factor))(exponent$prime));
      };
      var converted = Data_Functor.map(Data_List_Types.functorList)(convert)(v.value0);
      var units$prime = Data_Foldable.foldMap(Data_List_Types.foldableList)(monoidDerivedUnit)(Data_Tuple.fst)(converted);
      var conv = Data_Foldable.product(Data_List_Types.foldableList)(Data_Decimal.semiringDecimal)(Data_Functor.map(Data_List_Types.functorList)(Data_Tuple.snd)(converted));
      return new Data_Tuple.Tuple(units$prime, conv);
  };
  var baseRepresentation = function (du) {
      if (Data_Eq.eq(eqDerivedUnit)(du)(unity)) {
          return Data_List.singleton(du);
      };
      if (Data_Boolean.otherwise) {
          var replace = function (u) {
              var v = runDerivedUnit(u);
              if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
                  var b$prime = (function () {
                      var $216 = v["value0"]["baseUnit"]["long"] === "gram";
                      if ($216) {
                          return {
                              baseUnit: {
                                  "long": "kilogram",
                                  "short": "kg",
                                  unitType: v.value0.baseUnit.unitType
                              },
                              exponent: v.value0.exponent,
                              prefix: v.value0.prefix
                          };
                      };
                      return v.value0;
                  })();
                  return new DerivedUnit(Data_List.singleton(b$prime));
              };
              return u;
          };
          var du$prime = Data_Tuple.fst(toStandardUnit(du));
          var us = Data_Functor.map(Data_List_Types.functorList)(function ($237) {
              return replace(Data_Tuple.snd($237));
          })(splitByDimension(du$prime));
          return us;
      };
      throw new Error("Failed pattern match at Data.Units (line 291, column 1 - line 291, column 52): " + [ du.constructor.name ]);
  };
  var atto = decimalPrefix(-18.0);
  exports["removePrefix"] = removePrefix;
  exports["splitByDimension"] = splitByDimension;
  exports["baseRepresentation"] = baseRepresentation;
  exports["makeStandard"] = makeStandard;
  exports["makeNonStandard"] = makeNonStandard;
  exports["toStandardUnit"] = toStandardUnit;
  exports["toString"] = toString;
  exports["power"] = power;
  exports["divideUnits"] = divideUnits;
  exports["unity"] = unity;
  exports["atto"] = atto;
  exports["femto"] = femto;
  exports["pico"] = pico;
  exports["nano"] = nano;
  exports["micro"] = micro;
  exports["centi"] = centi;
  exports["deci"] = deci;
  exports["hecto"] = hecto;
  exports["milli"] = milli;
  exports["kilo"] = kilo;
  exports["mega"] = mega;
  exports["giga"] = giga;
  exports["tera"] = tera;
  exports["peta"] = peta;
  exports["exa"] = exa;
  exports["kibi"] = kibi;
  exports["mebi"] = mebi;
  exports["gibi"] = gibi;
  exports["tebi"] = tebi;
  exports["pebi"] = pebi;
  exports["exbi"] = exbi;
  exports["zebi"] = zebi;
  exports["yobi"] = yobi;
  exports["eqDerivedUnit"] = eqDerivedUnit;
  exports["semigroupDerivedUnit"] = semigroupDerivedUnit;
  exports["monoidDerivedUnit"] = monoidDerivedUnit;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.SI"] = $PS["Data.Units.SI"] || {};
  var exports = $PS["Data.Units.SI"];
  var Data_Units = $PS["Data.Units"];                
  var second = Data_Units.makeStandard("second")("s");
  var mole = Data_Units.makeStandard("mole")("mol");
  var meter = Data_Units.makeStandard("meter")("m");
  var kelvin = Data_Units.makeStandard("kelvin")("K");
  var gram = Data_Units.makeStandard("gram")("g");
  var kilogram = Data_Units.kilo(gram);
  var candela = Data_Units.makeStandard("candela")("cd");
  var ampere = Data_Units.makeStandard("ampere")("A");
  exports["meter"] = meter;
  exports["kilogram"] = kilogram;
  exports["second"] = second;
  exports["gram"] = gram;
  exports["ampere"] = ampere;
  exports["mole"] = mole;
  exports["kelvin"] = kelvin;
  exports["candela"] = candela;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.SI.Accepted"] = $PS["Data.Units.SI.Accepted"] || {};
  var exports = $PS["Data.Units.SI.Accepted"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];
  var $$Math = $PS["Math"];                
  var tonne = Data_Units.makeNonStandard("tonne")("ton")(1000000.0)(Data_Units_SI.gram);
  var liter = Data_Units.makeNonStandard("liter")("L")(1.0e-3)(Data_Units.power(Data_Units_SI.meter)(3.0));
  var hectare = Data_Units.makeNonStandard("hectare")("ha")(10000.0)(Data_Units.power(Data_Units_SI.meter)(2.0));
  var electronvolt = Data_Units.makeNonStandard("electronvolt")("eV")(1.60217653e-16)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Units.power(Data_Units_SI.second)(2.0))));
  var degree = Data_Units.makeNonStandard("degree")("\xb0")($$Math.pi / 180.0)(Data_Units.unity);
  var bel = Data_Units.makeStandard("bel")("bel");
  var barn = Data_Units.makeNonStandard("barn")("barn")(1.0e-28)(Data_Units.power(Data_Units_SI.meter)(2.0));
  var bar = Data_Units.makeNonStandard("bar")("bar")(1.0e8)(Data_Units.divideUnits(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.meter)(Data_Units.power(Data_Units_SI.second)(2.0))));
  var astronomicalUnit = Data_Units.makeNonStandard("astronomical unit")("AU")(1.495978707e11)(Data_Units_SI.meter);
  var angstrom = Data_Units.makeNonStandard("\xc5ngstr\xf6m")("\xc5")(1.0e-10)(Data_Units_SI.meter);
  exports["degree"] = degree;
  exports["hectare"] = hectare;
  exports["liter"] = liter;
  exports["tonne"] = tonne;
  exports["electronvolt"] = electronvolt;
  exports["bel"] = bel;
  exports["astronomicalUnit"] = astronomicalUnit;
  exports["bar"] = bar;
  exports["angstrom"] = angstrom;
  exports["barn"] = barn;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.SI.Derived"] = $PS["Data.Units.SI.Derived"] || {};
  var exports = $PS["Data.Units.SI.Derived"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var weber = Data_Units.makeNonStandard("weber")("Wb")(1000.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.second)(2.0))(Data_Units_SI.ampere))));
  var watt = Data_Units.makeNonStandard("watt")("W")(1000.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Units.power(Data_Units_SI.second)(3.0))));
  var volt = Data_Units.makeNonStandard("volt")("V")(1000.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.second)(3.0))(Data_Units_SI.ampere))));
  var tesla = Data_Units.makeNonStandard("tesla")("T")(1000.0)(Data_Units.divideUnits(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.second)(2.0))(Data_Units_SI.ampere)));
  var sievert = Data_Units.makeNonStandard("sievert")("Sv")(1.0)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Units.power(Data_Units_SI.second)(2.0)));
  var siemens = Data_Units.makeNonStandard("siemens")("S")(1.0e-3)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.second)(3.0))(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.ampere)(2.0))(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.power(Data_Units_SI.meter)(2.0)))));
  var radian = Data_Units.makeNonStandard("radian")("rad")(1.0)(Data_Units.unity);
  var pascal = Data_Units.makeNonStandard("pascal")("Pa")(1000.0)(Data_Units.divideUnits(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.meter)(Data_Units.power(Data_Units_SI.second)(2.0))));
  var ohm = Data_Units.makeNonStandard("ohm")("\u03a9")(1000.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.second)(3.0))(Data_Units.power(Data_Units_SI.ampere)(2.0)))));
  var newton = Data_Units.makeNonStandard("newton")("N")(1000.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.divideUnits(Data_Units_SI.meter)(Data_Units.power(Data_Units_SI.second)(2.0))));
  var lux = Data_Units.makeNonStandard("lux")("lx")(1.0)(Data_Units.divideUnits(Data_Units_SI.candela)(Data_Units.power(Data_Units_SI.meter)(2.0)));
  var lumen = Data_Units.makeNonStandard("lumen")("lm")(1.0)(Data_Units_SI.candela);
  var katal = Data_Units.makeNonStandard("katal")("kat")(1.0)(Data_Units.divideUnits(Data_Units_SI.mole)(Data_Units_SI.second));
  var joule = Data_Units.makeNonStandard("joule")("J")(1000.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Units.power(Data_Units_SI.second)(2.0))));
  var hertz = Data_Units.makeNonStandard("hertz")("Hz")(1.0)(Data_Units.divideUnits(Data_Units.unity)(Data_Units_SI.second));
  var henry = Data_Units.makeNonStandard("henry")("H")(1000.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.second)(2.0))(Data_Units.power(Data_Units_SI.ampere)(2.0)))));
  var gray = Data_Units.makeNonStandard("gray")("Gy")(1.0)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Units.power(Data_Units_SI.second)(2.0)));
  var farad = Data_Units.makeNonStandard("farad")("F")(1.0e-3)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.second)(4.0))(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.ampere)(2.0))(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.gram)(1.0))(Data_Units.power(Data_Units_SI.meter)(2.0)))));
  var coulomb = Data_Units.makeNonStandard("coulomb")("C")(1.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.ampere)(Data_Units_SI.second));
  var becquerel = Data_Units.makeNonStandard("becquerel")("Bq")(1.0)(Data_Units.power(Data_Units_SI.second)(-1.0));
  exports["radian"] = radian;
  exports["hertz"] = hertz;
  exports["newton"] = newton;
  exports["pascal"] = pascal;
  exports["joule"] = joule;
  exports["watt"] = watt;
  exports["coulomb"] = coulomb;
  exports["volt"] = volt;
  exports["farad"] = farad;
  exports["ohm"] = ohm;
  exports["siemens"] = siemens;
  exports["weber"] = weber;
  exports["tesla"] = tesla;
  exports["henry"] = henry;
  exports["lumen"] = lumen;
  exports["lux"] = lux;
  exports["becquerel"] = becquerel;
  exports["gray"] = gray;
  exports["sievert"] = sievert;
  exports["katal"] = katal;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Quantity"] = $PS["Data.Quantity"] || {};
  var exports = $PS["Data.Quantity"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Decimal = $PS["Data.Decimal"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI_Accepted = $PS["Data.Units.SI.Accepted"];
  var Data_Units_SI_Derived = $PS["Data.Units.SI.Derived"];                
  var Quantity = (function () {
      function Quantity(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Quantity.create = function (value0) {
          return function (value1) {
              return new Quantity(value0, value1);
          };
      };
      return Quantity;
  })();
  var ConversionError = (function () {
      function ConversionError(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      ConversionError.create = function (value0) {
          return function (value1) {
              return new ConversionError(value0, value1);
          };
      };
      return ConversionError;
  })();
  var value = function (v) {
      return v.value0;
  }; 
  var quantity$prime = function (n) {
      return function (du) {
          return new Quantity(n, du);
      };
  };
  var scalar$prime = function (factor) {
      return quantity$prime(factor)(Data_Units.unity);
  };
  var toStandard = function (v) {
      var v1 = Data_Units.toStandardUnit(v.value1);
      return quantity$prime(Data_Semiring.mul(Data_Decimal.semiringDecimal)(v1.value1)(v.value0))(v1.value0);
  };
  var quantity = function (n) {
      return function (du) {
          return new Quantity(Data_Decimal.fromNumber(n), du);
      };
  };
  var scalar = function (factor) {
      return quantity(factor)(Data_Units.unity);
  };
  var qNegate = function (v) {
      return new Quantity(Data_Ring.negate(Data_Decimal.ringDecimal)(v.value0), v.value1);
  };
  var qMultiply = function (v) {
      return function (v1) {
          return quantity$prime(Data_Semiring.mul(Data_Decimal.semiringDecimal)(v.value0)(v1.value0))(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(v.value1)(v1.value1));
      };
  };
  var qDivide = function (v) {
      return function (v1) {
          return quantity$prime(Data_EuclideanRing.div(Data_Decimal.euclideanRingDecimal)(v.value0)(v1.value0))(Data_Units.divideUnits(v.value1)(v1.value1));
      };
  };
  var prettyDecimal = function (d) {
      var $63 = Data_Decimal.isInteger(d) && Data_Ord.lessThan(Data_Decimal.ordDecimal)(d)(Data_Decimal.fromNumber(1.0e18));
      if ($63) {
          return Data_Decimal.toString(d);
      };
      return Data_Decimal.toString(Data_Decimal.toSignificantDigits(6)(d));
  };
  var prettyPrint$prime = function (v) {
      if (Data_Eq.eq(Data_Units.eqDerivedUnit)(v.value1)(Data_Units.unity)) {
          return {
              number: prettyDecimal(v.value0),
              space: false,
              unit: ""
          };
      };
      if (Data_Boolean.otherwise) {
          var space = Data_Eq.notEq(Data_Units.eqDerivedUnit)(v.value1)(Data_Units_SI_Accepted.degree);
          return {
              number: prettyDecimal(v.value0),
              space: space,
              unit: Data_Units.toString(v.value1)
          };
      };
      throw new Error("Failed pattern match at Data.Quantity (line 105, column 1 - line 105, column 78): " + [ v.constructor.name ]);
  };
  var pow = function (v) {
      return function (exp) {
          return quantity$prime(Data_Decimal.pow(v.value0)(exp))(Data_Units.power(v.value1)(Data_Decimal.toNumber(exp)));
      };
  };
  var sqrt = function (q) {
      return pow(q)(Data_Decimal.fromNumber(0.5));
  };
  var $$isFinite = function (v) {
      return Data_Decimal["isFinite"](v.value0);
  }; 
  var derivedUnit = function (v) {
      return v.value1;
  };
  var eqQuantity = new Data_Eq.Eq(function (q1) {
      return function (q2) {
          var q2$prime = toStandard(q2);
          var u2 = derivedUnit(q2$prime);
          var v2 = value(q2$prime);
          var q1$prime = toStandard(q1);
          var u1 = derivedUnit(q1$prime);
          var v1 = value(q1$prime);
          return Data_Eq.eq(Data_Decimal.eqDecimal)(v1)(v2) && Data_Eq.eq(Data_Units.eqDerivedUnit)(u1)(u2) || Data_Eq.eq(Data_Decimal.eqDecimal)(v1)(Data_Semiring.zero(Data_Decimal.semiringDecimal)) && Data_Eq.eq(Data_Decimal.eqDecimal)(v2)(Data_Semiring.zero(Data_Decimal.semiringDecimal));
      };
  });
  var convert = function (to) {
      return function (v) {
          if (Data_Eq.eq(Data_Units.eqDerivedUnit)(to)(v.value1)) {
              return new Data_Either.Right(new Quantity(v.value0, to));
          };
          if (Data_Eq.eq(Data_Decimal.eqDecimal)(v.value0)(Data_Semiring.zero(Data_Decimal.semiringDecimal))) {
              return new Data_Either.Right(new Quantity(Data_Semiring.zero(Data_Decimal.semiringDecimal), to));
          };
          if (Data_Boolean.otherwise) {
              var v1 = Data_Units.toStandardUnit(to);
              var q$prime = toStandard(v);
              var from$prime = derivedUnit(q$prime);
              var $97 = Data_Eq.eq(Data_Units.eqDerivedUnit)(from$prime)(v1.value0);
              if ($97) {
                  return Data_Either.Right.create(new Quantity(Data_EuclideanRing.div(Data_Decimal.euclideanRingDecimal)(q$prime.value0)(v1.value1), to));
              };
              return Data_Either.Left.create(new ConversionError(v.value1, to));
          };
          throw new Error("Failed pattern match at Data.Quantity (line 215, column 1 - line 215, column 67): " + [ to.constructor.name, v.constructor.name ]);
      };
  };
  var convertTo = Data_Function.flip(convert);
  var qAdd = function (v) {
      return function (v1) {
          if (Data_Eq.eq(Data_Decimal.eqDecimal)(v.value0)(Data_Semiring.zero(Data_Decimal.semiringDecimal))) {
              return Control_Applicative.pure(Data_Either.applicativeEither)(v1);
          };
          if (Data_Eq.eq(Data_Decimal.eqDecimal)(v1.value0)(Data_Semiring.zero(Data_Decimal.semiringDecimal))) {
              return Control_Applicative.pure(Data_Either.applicativeEither)(v);
          };
          if (Data_Boolean.otherwise) {
              return Control_Bind.bind(Data_Either.bindEither)(convertTo(v1)(v.value1))(function (q2$prime) {
                  return Control_Applicative.pure(Data_Either.applicativeEither)(quantity$prime(Data_Semiring.add(Data_Decimal.semiringDecimal)(v.value0)(q2$prime.value0))(v.value1));
              });
          };
          throw new Error("Failed pattern match at Data.Quantity (line 262, column 1 - line 262, column 61): " + [ v.constructor.name, v1.constructor.name ]);
      };
  };
  var qSubtract = function (q1) {
      return function (v) {
          return qAdd(q1)(new Quantity(Data_Ring.negate(Data_Decimal.ringDecimal)(v.value0), v.value1));
      };
  };
  var asValueIn$prime = function (u) {
      return Control_Bind.composeKleisli(Data_Either.bindEither)(convertTo(u))((function () {
          var $136 = Control_Applicative.pure(Data_Either.applicativeEither);
          return function ($137) {
              return $136(value($137));
          };
      })());
  };
  var toScalar$prime = function (q) {
      return asValueIn$prime(q)(Data_Units.unity);
  };
  var fullSimplify = function (v) {
      var v1 = toScalar$prime(v);
      if (v1 instanceof Data_Either.Right) {
          var $120 = Data_Eq.notEq(Data_Units.eqDerivedUnit)(Data_Units.removePrefix(v.value1))(Data_Units_SI_Accepted.degree) && Data_Eq.notEq(Data_Units.eqDerivedUnit)(Data_Units.removePrefix(v.value1))(Data_Units_SI_Derived.radian);
          if ($120) {
              return new Quantity(v1.value0, Data_Units.unity);
          };
          return quantity$prime(v.value0)(v.value1);
      };
      if (v1 instanceof Data_Either.Left) {
          var toTuple = function (v2) {
              var v3 = convertTo(quantity(1)(v2.value1))(v2.value0);
              if (v3 instanceof Data_Either.Right) {
                  return new Data_Tuple.Tuple(v3.value0.value0, v3.value0.value1);
              };
              if (v3 instanceof Data_Either.Left) {
                  return new Data_Tuple.Tuple(Data_Semiring.one(Data_Decimal.semiringDecimal), v2.value0);
              };
              throw new Error("Failed pattern match at Data.Quantity (line 151, column 13 - line 153, column 55): " + [ v3.constructor.name ]);
          };
          var list = Data_Units.splitByDimension(v.value1);
          var list$prime = Data_Functor.map(Data_List_Types.functorList)(toTuple)(list);
          var factor = Data_Foldable.product(Data_List_Types.foldableList)(Data_Decimal.semiringDecimal)(Data_Functor.map(Data_List_Types.functorList)(Data_Tuple.fst)(list$prime));
          var du$prime = Data_Foldable.foldMap(Data_List_Types.foldableList)(Data_Units.monoidDerivedUnit)(Data_Tuple.snd)(list$prime);
          return new Quantity(Data_Semiring.mul(Data_Decimal.semiringDecimal)(v.value0)(factor), du$prime);
      };
      throw new Error("Failed pattern match at Data.Quantity (line 142, column 3 - line 160, column 32): " + [ v1.constructor.name ]);
  };
  var asValueIn = function (q) {
      return function (u) {
          return Data_Functor.map(Data_Either.functorEither)(Data_Decimal.toNumber)(asValueIn$prime(q)(u));
      };
  };
  var toScalar = function (q) {
      return asValueIn(q)(Data_Units.unity);
  };
  var abs = function (v) {
      return quantity$prime(Data_Decimal.abs(v.value0))(v.value1);
  };
  exports["quantity"] = quantity;
  exports["quantity'"] = quantity$prime;
  exports["prettyPrint'"] = prettyPrint$prime;
  exports["derivedUnit"] = derivedUnit;
  exports["fullSimplify"] = fullSimplify;
  exports["scalar"] = scalar;
  exports["scalar'"] = scalar$prime;
  exports["convertTo"] = convertTo;
  exports["asValueIn"] = asValueIn;
  exports["asValueIn'"] = asValueIn$prime;
  exports["toScalar"] = toScalar;
  exports["toScalar'"] = toScalar$prime;
  exports["isFinite"] = $$isFinite;
  exports["qNegate"] = qNegate;
  exports["qAdd"] = qAdd;
  exports["qSubtract"] = qSubtract;
  exports["qMultiply"] = qMultiply;
  exports["qDivide"] = qDivide;
  exports["pow"] = pow;
  exports["abs"] = abs;
  exports["sqrt"] = sqrt;
  exports["eqQuantity"] = eqQuantity;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Quantity.Math"] = $PS["Data.Quantity.Math"] || {};
  var exports = $PS["Data.Quantity.Math"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Decimal = $PS["Data.Decimal"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Quantity = $PS["Data.Quantity"];
  var Data_Semiring = $PS["Data.Semiring"];                
  var tau = Data_Quantity["scalar'"](Data_Semiring.mul(Data_Decimal.semiringDecimal)(Data_Decimal.fromNumber(2.0))(Data_Decimal.pi));
  var pi = Data_Quantity["scalar'"](Data_Decimal.pi);
  var mean = function (xs) {
      var n = Data_Quantity["scalar'"](Data_Decimal.fromInt(Data_List_NonEmpty.length(xs)));
      return Data_Functor.map(Data_Either.functorEither)(function (v) {
          return Data_Quantity.qDivide(v)(n);
      })(Data_Foldable.foldM(Data_List_Types.foldableList)(Data_Either.monadEither)(Data_Quantity.qAdd)(Data_List_NonEmpty.head(xs))(Data_List_NonEmpty.tail(xs)));
  };
  var lift2 = function (f) {
      return function (q1) {
          return function (q2) {
              var u = Data_Quantity.derivedUnit(q1);
              return Control_Bind.bind(Data_Either.bindEither)(Data_Quantity["asValueIn'"](q1)(u))(function (v1) {
                  return Control_Bind.bind(Data_Either.bindEither)(Data_Quantity["asValueIn'"](q2)(u))(function (v2) {
                      return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity["quantity'"](f(v1)(v2))(u));
                  });
              });
          };
      };
  };
  var max2 = lift2(Data_Decimal.max);
  var max = function (xs) {
      return Data_Foldable.foldM(Data_List_Types.foldableList)(Data_Either.monadEither)(max2)(Data_List_NonEmpty.head(xs))(Data_List_NonEmpty.tail(xs));
  };
  var min2 = lift2(Data_Decimal.min);
  var min = function (xs) {
      return Data_Foldable.foldM(Data_List_Types.foldableList)(Data_Either.monadEither)(min2)(Data_List_NonEmpty.head(xs))(Data_List_NonEmpty.tail(xs));
  };
  var modulo = lift2(Data_Decimal.modulo);
  var lift = function (fn) {
      return function (q) {
          return Data_Functor.map(Data_Either.functorEither)(function ($1) {
              return Data_Quantity["scalar'"](fn($1));
          })(Data_Quantity["toScalar'"](q));
      };
  };
  var ln = lift(Data_Decimal.ln);
  var log10 = lift(Data_Decimal.log10);
  var round = lift(Data_Decimal.round);
  var sin = lift(Data_Decimal.sin);
  var sinh = lift(Data_Decimal.sinh);
  var tan = lift(Data_Decimal.tan);
  var tanh = lift(Data_Decimal.tanh);
  var gamma = lift(Data_Decimal.gamma);
  var floor = lift(Data_Decimal.floor);
  var factorial = lift(Data_Decimal.factorial);
  var exp = lift(Data_Decimal.exp);
  var e = Data_Quantity["scalar'"](Data_Decimal.e);
  var cosh = lift(Data_Decimal.cosh);
  var cos = lift(Data_Decimal.cos);
  var ceil = lift(Data_Decimal.ceil);
  var atanh = lift(Data_Decimal.atanh);
  var atan2 = function (x) {
      return function (y) {
          var removeDims = function (q) {
              return Data_Quantity.qDivide(q)(Data_Quantity.quantity(1.0)(Data_Quantity.derivedUnit(q)));
          };
          return Data_Functor.map(Data_Either.functorEither)(removeDims)(lift2(Data_Decimal.atan2)(x)(y));
      };
  };
  var atan = lift(Data_Decimal.atan);
  var asinh = lift(Data_Decimal.asinh);
  var asin = lift(Data_Decimal.asin);
  var acosh = lift(Data_Decimal.acosh);
  var acos = lift(Data_Decimal.acos);
  exports["acos"] = acos;
  exports["asin"] = asin;
  exports["atan"] = atan;
  exports["atan2"] = atan2;
  exports["cos"] = cos;
  exports["exp"] = exp;
  exports["ln"] = ln;
  exports["sin"] = sin;
  exports["tan"] = tan;
  exports["sinh"] = sinh;
  exports["cosh"] = cosh;
  exports["tanh"] = tanh;
  exports["asinh"] = asinh;
  exports["acosh"] = acosh;
  exports["atanh"] = atanh;
  exports["ceil"] = ceil;
  exports["floor"] = floor;
  exports["log10"] = log10;
  exports["max"] = max;
  exports["min"] = min;
  exports["mean"] = mean;
  exports["modulo"] = modulo;
  exports["round"] = round;
  exports["gamma"] = gamma;
  exports["factorial"] = factorial;
  exports["pi"] = pi;
  exports["e"] = e;
  exports["tau"] = tau;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Quantity.Physics"] = $PS["Data.Quantity.Physics"] || {};
  var exports = $PS["Data.Quantity.Physics"];
  var Data_Decimal = $PS["Data.Decimal"];
  var Data_Either = $PS["Data.Either"];
  var Data_Quantity = $PS["Data.Quantity"];
  var Data_Quantity_Math = $PS["Data.Quantity.Math"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];
  var Data_Units_SI_Derived = $PS["Data.Units.SI.Derived"];                
  var µB = Data_Quantity.quantity(9.274009994e-24)(Data_Units.divideUnits(Data_Units_SI_Derived.joule)(Data_Units_SI_Derived.tesla));
  var µ0 = Data_Quantity.qMultiply(Data_Quantity_Math.pi)(Data_Quantity.quantity(4.0e-7)(Data_Units.divideUnits(Data_Units_SI_Derived.newton)(Data_Units.power(Data_Units_SI.ampere)(2.0))));
  var speedOfLight = Data_Quantity.quantity(2.99792458e8)(Data_Units.divideUnits(Data_Units_SI.meter)(Data_Units_SI.second));
  var ε0 = (function () {
      var ε0$prime = Data_Quantity.qDivide(Data_Quantity.scalar(1.0))(Data_Quantity.qMultiply(µ0)(Data_Quantity.pow(speedOfLight)(Data_Decimal.fromNumber(2.0))));
      var v = Data_Quantity.convertTo(ε0$prime)(Data_Units.divideUnits(Data_Units_SI_Derived.farad)(Data_Units_SI.meter));
      if (v instanceof Data_Either.Right) {
          return v.value0;
      };
      if (v instanceof Data_Either.Left) {
          return ε0$prime;
      };
      throw new Error("Failed pattern match at Data.Quantity.Physics (line 60, column 6 - line 62, column 20): " + [ v.constructor.name ]);
  })();
  var protonMass = Data_Quantity.quantity(1.672621898e-27)(Data_Units_SI.kilogram);
  var planckConstant = Data_Quantity.quantity(6.62607004e-34)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI_Derived.joule)(Data_Units_SI.second));
  var ℏ = Data_Quantity.qDivide(planckConstant)(Data_Quantity.qMultiply(Data_Quantity.scalar(2.0))(Data_Quantity_Math.pi));
  var kB = Data_Quantity.quantity(1.38064852e-23)(Data_Units.divideUnits(Data_Units_SI_Derived.joule)(Data_Units_SI.kelvin));
  var idealGasConstant = Data_Quantity.quantity(8.3145)(Data_Units.divideUnits(Data_Units_SI_Derived.joule)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.mole)(Data_Units_SI.kelvin)));
  var gravitationalConstant = Data_Quantity.quantity(6.67408e-11)(Data_Units.divideUnits(Data_Units.power(Data_Units_SI.meter)(3.0))(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.kilo(Data_Units_SI.gram))(Data_Units.power(Data_Units_SI.second)(2.0))));
  var g0 = Data_Quantity.quantity(9.80665)(Data_Units.divideUnits(Data_Units_SI.meter)(Data_Units.power(Data_Units_SI.second)(2.0)));
  var electronMass = Data_Quantity.quantity(9.1093826e-31)(Data_Units.kilo(Data_Units_SI.gram));
  var electronCharge = Data_Quantity.quantity(1.60217653e-19)(Data_Units_SI_Derived.coulomb);
  var α = Data_Quantity.fullSimplify(Data_Quantity.qDivide(Data_Quantity.pow(electronCharge)(Data_Decimal.fromNumber(2.0)))(Data_Quantity.qMultiply(Data_Quantity.qMultiply(Data_Quantity.qMultiply(Data_Quantity.qMultiply(Data_Quantity.scalar(4.0))(Data_Quantity_Math.pi))(ε0))(ℏ))(speedOfLight)));
  var avogadroConstant = Data_Quantity.quantity(6.022140857e23)(Data_Units.power(Data_Units_SI.mole)(-1.0));
  exports["speedOfLight"] = speedOfLight;
  exports["gravitationalConstant"] = gravitationalConstant;
  exports["planckConstant"] = planckConstant;
  exports["ℏ"] = ℏ;
  exports["electronMass"] = electronMass;
  exports["electronCharge"] = electronCharge;
  exports["µ0"] = µ0;
  exports["ε0"] = ε0;
  exports["µB"] = µB;
  exports["α"] = α;
  exports["protonMass"] = protonMass;
  exports["avogadroConstant"] = avogadroConstant;
  exports["kB"] = kB;
  exports["g0"] = g0;
  exports["idealGasConstant"] = idealGasConstant;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Set"] = $PS["Data.Set"] || {};
  var exports = $PS["Data.Set"];
  var Data_List = $PS["Data.List"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var toList = function (v) {
      return Data_Map_Internal.keys(v);
  };
  var toUnfoldable = function (dictUnfoldable) {
      var $63 = Data_List.toUnfoldable(dictUnfoldable);
      return function ($64) {
          return $63(toList($64));
      };
  };
  exports["toUnfoldable"] = toUnfoldable;
})(PS);
(function(exports) {
  "use strict";
  /* global Symbol */

  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator =
    typeof Symbol !== "undefined" &&
    Symbol != null &&
    typeof Symbol.iterator !== "undefined" &&
    typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";

  exports._unsafeCodePointAt0 = function (fallback) {
    return hasCodePointAt
      ? function (str) { return str.codePointAt(0); }
      : fallback;
  };

  exports._fromCodePointArray = function (singleton) {
    return hasFromCodePoint
      ? function (cps) {
        // Function.prototype.apply will fail for very large second parameters,
        // so we don't use it for arrays with 10,000 or more entries.
        if (cps.length < 10e3) {
          return String.fromCodePoint.apply(String, cps);
        }
        return cps.map(singleton).join("");
      }
      : function (cps) {
        return cps.map(singleton).join("");
      };
  };

  exports._singleton = function (fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };

  exports._take = function (fallback) {
    return function (n) {
      if (hasStringIterator) {
        return function (str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i = 0; i < n; ++i) {
            var o = iter.next();
            if (o.done) return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };

  exports._toCodePointArray = function (fallback) {
    return function (unsafeCodePointAt0) {
      if (hasArrayFrom) {
        return function (str) {
          return Array.from(str, unsafeCodePointAt0);
        };
      }
      return fallback;
    };
  };
})(PS["Data.String.CodePoints"] = PS["Data.String.CodePoints"] || {});
(function(exports) {
  "use strict";

  exports.fromCharArray = function (a) {
    return a.join("");
  };

  exports.toCharArray = function (s) {
    return s.split("");
  };

  exports.singleton = function (c) {
    return c;
  };

  exports._charAt = function (just) {
    return function (nothing) {
      return function (i) {
        return function (s) {
          return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
        };
      };
    };
  };

  exports.length = function (s) {
    return s.length;
  };

  exports._indexOf = function (just) {
    return function (nothing) {
      return function (x) {
        return function (s) {
          var i = s.indexOf(x);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };

  exports.take = function (n) {
    return function (s) {
      return s.substr(0, n);
    };
  };

  exports.drop = function (n) {
    return function (s) {
      return s.substring(n);
    };
  };
})(PS["Data.String.CodeUnits"] = PS["Data.String.CodeUnits"] || {});
(function(exports) {
  "use strict";

  exports.charAt = function (i) {
    return function (s) {
      if (i >= 0 && i < s.length) return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };
})(PS["Data.String.Unsafe"] = PS["Data.String.Unsafe"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.String.Unsafe"] = $PS["Data.String.Unsafe"] || {};
  var exports = $PS["Data.String.Unsafe"];
  var $foreign = $PS["Data.String.Unsafe"];
  exports["charAt"] = $foreign.charAt;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.String.CodeUnits"] = $PS["Data.String.CodeUnits"] || {};
  var exports = $PS["Data.String.CodeUnits"];
  var $foreign = $PS["Data.String.CodeUnits"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_String_Unsafe = $PS["Data.String.Unsafe"];                
  var uncons = function (v) {
      if (v === "") {
          return Data_Maybe.Nothing.value;
      };
      return new Data_Maybe.Just({
          head: Data_String_Unsafe.charAt(0)(v),
          tail: $foreign.drop(1)(v)
      });
  };                                                                                          
  var indexOf = $foreign["_indexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var charAt = $foreign["_charAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["uncons"] = uncons;
  exports["indexOf"] = indexOf;
  exports["singleton"] = $foreign.singleton;
  exports["fromCharArray"] = $foreign.fromCharArray;
  exports["toCharArray"] = $foreign.toCharArray;
  exports["length"] = $foreign.length;
  exports["take"] = $foreign.take;
  exports["drop"] = $foreign.drop;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.String.CodePoints"] = $PS["Data.String.CodePoints"] || {};
  var exports = $PS["Data.String.CodePoints"];
  var $foreign = $PS["Data.String.CodePoints"];
  var Data_Array = $PS["Data.Array"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Data_String_Unsafe = $PS["Data.String.Unsafe"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];                
  var CodePoint = function (x) {
      return x;
  };
  var unsurrogate = function (lead) {
      return function (trail) {
          return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
      };
  }; 
  var isTrail = function (cu) {
      return 56320 <= cu && cu <= 57343;
  };
  var isLead = function (cu) {
      return 55296 <= cu && cu <= 56319;
  };
  var uncons = function (s) {
      var v = Data_String_CodeUnits.length(s);
      if (v === 0) {
          return Data_Maybe.Nothing.value;
      };
      if (v === 1) {
          return new Data_Maybe.Just({
              head: Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(0)(s)),
              tail: ""
          });
      };
      var cu1 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(1)(s));
      var cu0 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(0)(s));
      var $21 = isLead(cu0) && isTrail(cu1);
      if ($21) {
          return new Data_Maybe.Just({
              head: unsurrogate(cu0)(cu1),
              tail: Data_String_CodeUnits.drop(2)(s)
          });
      };
      return new Data_Maybe.Just({
          head: cu0,
          tail: Data_String_CodeUnits.drop(1)(s)
      });
  };
  var unconsButWithTuple = function (s) {
      return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
          return new Data_Tuple.Tuple(v.head, v.tail);
      })(uncons(s));
  };
  var toCodePointArrayFallback = function (s) {
      return Data_Unfoldable.unfoldr(Data_Unfoldable.unfoldableArray)(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function (s) {
      var cu0 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(0)(s));
      var $25 = isLead(cu0) && Data_String_CodeUnits.length(s) > 1;
      if ($25) {
          var cu1 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(1)(s));
          var $26 = isTrail(cu1);
          if ($26) {
              return unsurrogate(cu0)(cu1);
          };
          return cu0;
      };
      return cu0;
  };
  var unsafeCodePointAt0 = $foreign["_unsafeCodePointAt0"](unsafeCodePointAt0Fallback);
  var toCodePointArray = $foreign["_toCodePointArray"](toCodePointArrayFallback)(unsafeCodePointAt0);
  var length = function ($52) {
      return Data_Array.length(toCodePointArray($52));
  };
  var indexOf = function (p) {
      return function (s) {
          return Data_Functor.map(Data_Maybe.functorMaybe)(function (i) {
              return length(Data_String_CodeUnits.take(i)(s));
          })(Data_String_CodeUnits.indexOf(p)(s));
      };
  };
  var fromCharCode = (function () {
      var $53 = Data_Enum.toEnumWithDefaults(Data_Enum.boundedEnumChar)(Data_Bounded.bottom(Data_Bounded.boundedChar))(Data_Bounded.top(Data_Bounded.boundedChar));
      return function ($54) {
          return Data_String_CodeUnits.singleton($53($54));
      };
  })();
  var singletonFallback = function (v) {
      if (v <= 65535) {
          return fromCharCode(v);
      };
      var lead = Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(v - 65536 | 0)(1024) + 55296 | 0;
      var trail = Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(v - 65536 | 0)(1024) + 56320 | 0;
      return fromCharCode(lead) + fromCharCode(trail);
  };
  var fromCodePointArray = $foreign["_fromCodePointArray"](singletonFallback);
  var singleton = $foreign["_singleton"](singletonFallback);
  var takeFallback = function (n) {
      return function (v) {
          if (n < 1) {
              return "";
          };
          var v1 = uncons(v);
          if (v1 instanceof Data_Maybe.Just) {
              return singleton(v1.value0.head) + takeFallback(n - 1 | 0)(v1.value0.tail);
          };
          return v;
      };
  };
  var take = $foreign["_take"](takeFallback);
  var drop = function (n) {
      return function (s) {
          return Data_String_CodeUnits.drop(Data_String_CodeUnits.length(take(n)(s)))(s);
      };
  };
  var codePointFromChar = (function () {
      var $55 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar);
      return function ($56) {
          return CodePoint($55($56));
      };
  })();
  exports["codePointFromChar"] = codePointFromChar;
  exports["singleton"] = singleton;
  exports["fromCodePointArray"] = fromCodePointArray;
  exports["length"] = length;
  exports["indexOf"] = indexOf;
  exports["drop"] = drop;
})(PS);
(function(exports) {
  "use strict";

  exports.split = function (sep) {
    return function (s) {
      return s.split(sep);
    };
  };

  exports.toLower = function (s) {
    return s.toLowerCase();
  };
})(PS["Data.String.Common"] = PS["Data.String.Common"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.String.Common"] = $PS["Data.String.Common"] || {};
  var exports = $PS["Data.String.Common"];
  var $foreign = $PS["Data.String.Common"];                
  var $$null = function (s) {
      return s === "";
  };
  exports["null"] = $$null;
  exports["split"] = $foreign.split;
  exports["toLower"] = $foreign.toLower;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.String.Pattern"] = $PS["Data.String.Pattern"] || {};
  var exports = $PS["Data.String.Pattern"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Pattern = function (x) {
      return x;
  };              
  var newtypePattern = new Data_Newtype.Newtype(function (n) {
      return n;
  }, Pattern);
  exports["newtypePattern"] = newtypePattern;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.Astronomical"] = $PS["Data.Units.Astronomical"] || {};
  var exports = $PS["Data.Units.Astronomical"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var parsec = Data_Units.makeNonStandard("parsec")("parsec")(3.085677581e16)(Data_Units_SI.meter);
  var lightyear = Data_Units.makeNonStandard("lightyear")("ly")(9.4607304725808e15)(Data_Units_SI.meter);
  exports["parsec"] = parsec;
  exports["lightyear"] = lightyear;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.Bit"] = $PS["Data.Units.Bit"] || {};
  var exports = $PS["Data.Units.Bit"];
  var Data_Units = $PS["Data.Units"];                
  var bit = Data_Units.makeStandard("bit")("bit");
  var $$byte = Data_Units.makeNonStandard("byte")("B")(8.0)(bit);
  exports["bit"] = bit;
  exports["byte"] = $$byte;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.CGS"] = $PS["Data.Units.CGS"] || {};
  var exports = $PS["Data.Units.CGS"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var gauss = Data_Units.makeNonStandard("gauss")("gauss")(0.1)(Data_Units.divideUnits(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.second)(2.0))(Data_Units_SI.ampere)));
  exports["gauss"] = gauss;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.Currency"] = $PS["Data.Units.Currency"] || {};
  var exports = $PS["Data.Units.Currency"];
  var Data_Units = $PS["Data.Units"];                
  var euro = Data_Units.makeStandard("euro")("EUR");
  var dollar = Data_Units.makeStandard("dollar")("USD");
  exports["dollar"] = dollar;
  exports["euro"] = euro;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.Imperial"] = $PS["Data.Units.Imperial"] || {};
  var exports = $PS["Data.Units.Imperial"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var yard = Data_Units.makeNonStandard("yard")("yd")(0.9144)(Data_Units_SI.meter);
  var thou = Data_Units.makeNonStandard("thou")("thou")(2.54e-5)(Data_Units_SI.meter);
  var pound = Data_Units.makeNonStandard("pound")("lb")(453.6)(Data_Units_SI.gram);
  var ounce = Data_Units.makeNonStandard("ounce")("oz")(28.35)(Data_Units_SI.gram);
  var mile = Data_Units.makeNonStandard("mile")("mi")(1609.344)(Data_Units_SI.meter);
  var inch = Data_Units.makeNonStandard("inch")("in")(2.54e-2)(Data_Units_SI.meter);
  var furlong = Data_Units.makeNonStandard("furlong")("furlong")(201.168)(Data_Units_SI.meter);
  var foot = Data_Units.makeNonStandard("foot")("ft")(0.3048)(Data_Units_SI.meter);
  exports["inch"] = inch;
  exports["foot"] = foot;
  exports["yard"] = yard;
  exports["mile"] = mile;
  exports["ounce"] = ounce;
  exports["pound"] = pound;
  exports["thou"] = thou;
  exports["furlong"] = furlong;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.Misc"] = $PS["Data.Units.Misc"] || {};
  var exports = $PS["Data.Units.Misc"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var rpm = Data_Units.makeNonStandard("rpm")("rpm")(1.0 / 60.0)(Data_Units.power(Data_Units_SI.second)(-1.0));
  var psi = Data_Units.makeNonStandard("psi")("psi")(6894757.0)(Data_Units.divideUnits(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.meter)(Data_Units.power(Data_Units_SI.second)(2.0))));
  var pixel = Data_Units.makeStandard("pixel")("px");
  var piece = Data_Units.makeStandard("piece")("piece");
  var person = Data_Units.makeStandard("person")("person");
  var mmHg = Data_Units.makeNonStandard("mmHg")("mmHg")(133322.387415)(Data_Units.divideUnits(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.meter)(Data_Units.power(Data_Units_SI.second)(2.0))));
  var lbf = Data_Units.makeNonStandard("pound_force")("lbf")(4448.222)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.meter)(Data_Units.power(Data_Units_SI.second)(-2.0))));
  var frame = Data_Units.makeStandard("frame")("frame");
  var fortnight = Data_Units.makeNonStandard("fortnight")("fortnight")(14.0 * 24.0 * 3600.0)(Data_Units_SI.second);
  var dot = Data_Units.makeStandard("dot")("dot");
  var calorie = Data_Units.makeNonStandard("calorie")("cal")(4184.0)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Units.power(Data_Units_SI.second)(-2.0))));
  var btu = Data_Units.makeNonStandard("BTU")("BTU")(1055055.85262)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units.power(Data_Units_SI.meter)(2.0))(Data_Units.power(Data_Units_SI.second)(-2.0))));
  var atm = Data_Units.makeNonStandard("atm")("atm")(1.01325e8)(Data_Units.divideUnits(Data_Units_SI.gram)(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI.meter)(Data_Units.power(Data_Units_SI.second)(2.0))));
  exports["calorie"] = calorie;
  exports["btu"] = btu;
  exports["lbf"] = lbf;
  exports["rpm"] = rpm;
  exports["fortnight"] = fortnight;
  exports["mmHg"] = mmHg;
  exports["psi"] = psi;
  exports["atm"] = atm;
  exports["pixel"] = pixel;
  exports["dot"] = dot;
  exports["frame"] = frame;
  exports["piece"] = piece;
  exports["person"] = person;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.Nautical"] = $PS["Data.Units.Nautical"] || {};
  var exports = $PS["Data.Units.Nautical"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var nauticalMile = Data_Units.makeNonStandard("nautical mile")("M")(1852.0)(Data_Units_SI.meter);
  var knot = Data_Units.makeNonStandard("knot")("kn")(1852.0 / 3600.0)(Data_Units.divideUnits(Data_Units_SI.meter)(Data_Units_SI.second));
  exports["knot"] = knot;
  exports["nauticalMile"] = nauticalMile;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.PartsPerX"] = $PS["Data.Units.PartsPerX"] || {};
  var exports = $PS["Data.Units.PartsPerX"];
  var Data_Units = $PS["Data.Units"];                
  var percent = Data_Units.makeNonStandard("percent")("pct")(1.0e-2)(Data_Units.unity);
  var partsPerTrillion = Data_Units.makeNonStandard("parts-per-trillion")("ppt")(1.0e-12)(Data_Units.unity);
  var partsPerQuadrillion = Data_Units.makeNonStandard("parts-per-quadrillion")("ppq")(1.0e-15)(Data_Units.unity);
  var partsPerMillion = Data_Units.makeNonStandard("parts-per-million")("ppm")(1.0e-6)(Data_Units.unity);
  var partsPerBillion = Data_Units.makeNonStandard("parts-per-billion")("ppb")(1.0e-9)(Data_Units.unity);
  exports["percent"] = percent;
  exports["partsPerMillion"] = partsPerMillion;
  exports["partsPerBillion"] = partsPerBillion;
  exports["partsPerTrillion"] = partsPerTrillion;
  exports["partsPerQuadrillion"] = partsPerQuadrillion;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.Time"] = $PS["Data.Units.Time"] || {};
  var exports = $PS["Data.Units.Time"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var year = Data_Units.makeNonStandard("year")("year")(365.25 * 24.0 * 3600.0)(Data_Units_SI.second);
  var week = Data_Units.makeNonStandard("week")("week")(7.0 * 24.0 * 3600.0)(Data_Units_SI.second);
  var month = Data_Units.makeNonStandard("month")("month")((30.0 * 24.0 + 10.5) * 3600.0)(Data_Units_SI.second);
  var minute = Data_Units.makeNonStandard("minute")("min")(60.0)(Data_Units_SI.second);
  var hour = Data_Units.makeNonStandard("hour")("h")(3600.0)(Data_Units_SI.second);
  var day = Data_Units.makeNonStandard("day")("d")(24.0 * 3600.0)(Data_Units_SI.second);
  exports["minute"] = minute;
  exports["hour"] = hour;
  exports["day"] = day;
  exports["week"] = week;
  exports["month"] = month;
  exports["year"] = year;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Units.USCustomary"] = $PS["Data.Units.USCustomary"] || {};
  var exports = $PS["Data.Units.USCustomary"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var teaspoon = Data_Units.makeNonStandard("teaspoon")("teaspoon")(4.92892159375e-6)(Data_Units.power(Data_Units_SI.meter)(3.0));
  var tablespoon = Data_Units.makeNonStandard("tablespoon")("tablespoon")(1.478676478125e-5)(Data_Units.power(Data_Units_SI.meter)(3.0));
  var rod = Data_Units.makeNonStandard("rod")("rod")(5.0292)(Data_Units_SI.meter);
  var pint = Data_Units.makeNonStandard("pint")("pint")(4.73176473e-4)(Data_Units.power(Data_Units_SI.meter)(3.0));
  var hogshead = Data_Units.makeNonStandard("hogshead")("hogshead")(0.238480942392)(Data_Units.power(Data_Units_SI.meter)(3.0));
  var gallon = Data_Units.makeNonStandard("gallon")("gal")(3.785411784e-3)(Data_Units.power(Data_Units_SI.meter)(3.0));
  var fluidounce = Data_Units.makeNonStandard("fluidounce")("floz")(2.95735295625e-5)(Data_Units.power(Data_Units_SI.meter)(3.0));
  var cup = Data_Units.makeNonStandard("cup")("cup")(2.365882365e-4)(Data_Units.power(Data_Units_SI.meter)(3.0));
  exports["gallon"] = gallon;
  exports["pint"] = pint;
  exports["cup"] = cup;
  exports["tablespoon"] = tablespoon;
  exports["teaspoon"] = teaspoon;
  exports["fluidounce"] = fluidounce;
  exports["hogshead"] = hogshead;
  exports["rod"] = rod;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Insect.Functions"] = $PS["Insect.Functions"] || {};
  var exports = $PS["Insect.Functions"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Either = $PS["Data.Either"];
  var Data_Quantity = $PS["Data.Quantity"];
  var Data_Units_SI = $PS["Data.Units.SI"];                
  var offsetFahrenheit = 459.67;
  var offsetCelsius = 273.15;
  var toCelsius = function (tempKelvin$prime) {
      return Control_Bind.bind(Data_Either.bindEither)(Data_Quantity.asValueIn(tempKelvin$prime)(Data_Units_SI.kelvin))(function (tempKelvin) {
          return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.scalar(tempKelvin - offsetCelsius));
      });
  };
  var multiplierFahrenheit = 5.0 / 9.0;
  var toFahrenheit = function (tempKelvin$prime) {
      return Control_Bind.bind(Data_Either.bindEither)(Data_Quantity.asValueIn(tempKelvin$prime)(Data_Units_SI.kelvin))(function (tempKelvin) {
          return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.scalar(tempKelvin / multiplierFahrenheit - offsetFahrenheit));
      });
  };
  var fromFahrenheit = function (tempFahrenheit$prime) {
      return Control_Bind.bind(Data_Either.bindEither)(Data_Quantity.toScalar(tempFahrenheit$prime))(function (tempFahrenheit) {
          return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.quantity((tempFahrenheit + offsetFahrenheit) * multiplierFahrenheit)(Data_Units_SI.kelvin));
      });
  };
  var fromCelsius = function (tempCelsius$prime) {
      return Control_Bind.bind(Data_Either.bindEither)(Data_Quantity.toScalar(tempCelsius$prime))(function (tempCelsius) {
          return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.quantity(tempCelsius + offsetCelsius)(Data_Units_SI.kelvin));
      });
  };
  exports["fromCelsius"] = fromCelsius;
  exports["toCelsius"] = toCelsius;
  exports["fromFahrenheit"] = fromFahrenheit;
  exports["toFahrenheit"] = toFahrenheit;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Insect.Language"] = $PS["Insect.Language"] || {};
  var exports = $PS["Insect.Language"];              
  var QConversionError = (function () {
      function QConversionError(value0) {
          this.value0 = value0;
      };
      QConversionError.create = function (value0) {
          return new QConversionError(value0);
      };
      return QConversionError;
  })();
  var WrongArityError = (function () {
      function WrongArityError(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      WrongArityError.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new WrongArityError(value0, value1, value2);
              };
          };
      };
      return WrongArityError;
  })();
  var LookupError = (function () {
      function LookupError(value0) {
          this.value0 = value0;
      };
      LookupError.create = function (value0) {
          return new LookupError(value0);
      };
      return LookupError;
  })();
  var NumericalError = (function () {
      function NumericalError() {

      };
      NumericalError.value = new NumericalError();
      return NumericalError;
  })();
  var RedefinedConstantError = (function () {
      function RedefinedConstantError(value0) {
          this.value0 = value0;
      };
      RedefinedConstantError.create = function (value0) {
          return new RedefinedConstantError(value0);
      };
      return RedefinedConstantError;
  })();
  var InvalidIdentifier = (function () {
      function InvalidIdentifier(value0) {
          this.value0 = value0;
      };
      InvalidIdentifier.create = function (value0) {
          return new InvalidIdentifier(value0);
      };
      return InvalidIdentifier;
  })();
  var Help = (function () {
      function Help() {

      };
      Help.value = new Help();
      return Help;
  })();
  var Reset = (function () {
      function Reset() {

      };
      Reset.value = new Reset();
      return Reset;
  })();
  var List = (function () {
      function List() {

      };
      List.value = new List();
      return List;
  })();
  var Clear = (function () {
      function Clear() {

      };
      Clear.value = new Clear();
      return Clear;
  })();
  var Copy = (function () {
      function Copy() {

      };
      Copy.value = new Copy();
      return Copy;
  })();
  var Quit = (function () {
      function Quit() {

      };
      Quit.value = new Quit();
      return Quit;
  })();
  var Add = (function () {
      function Add() {

      };
      Add.value = new Add();
      return Add;
  })();
  var Sub = (function () {
      function Sub() {

      };
      Sub.value = new Sub();
      return Sub;
  })();
  var Mul = (function () {
      function Mul() {

      };
      Mul.value = new Mul();
      return Mul;
  })();
  var Div = (function () {
      function Div() {

      };
      Div.value = new Div();
      return Div;
  })();
  var Pow = (function () {
      function Pow() {

      };
      Pow.value = new Pow();
      return Pow;
  })();
  var Mod = (function () {
      function Mod() {

      };
      Mod.value = new Mod();
      return Mod;
  })();
  var ConvertTo = (function () {
      function ConvertTo() {

      };
      ConvertTo.value = new ConvertTo();
      return ConvertTo;
  })();
  var Scalar = (function () {
      function Scalar(value0) {
          this.value0 = value0;
      };
      Scalar.create = function (value0) {
          return new Scalar(value0);
      };
      return Scalar;
  })();
  var Unit = (function () {
      function Unit(value0) {
          this.value0 = value0;
      };
      Unit.create = function (value0) {
          return new Unit(value0);
      };
      return Unit;
  })();
  var Variable = (function () {
      function Variable(value0) {
          this.value0 = value0;
      };
      Variable.create = function (value0) {
          return new Variable(value0);
      };
      return Variable;
  })();
  var Factorial = (function () {
      function Factorial(value0) {
          this.value0 = value0;
      };
      Factorial.create = function (value0) {
          return new Factorial(value0);
      };
      return Factorial;
  })();
  var Negate = (function () {
      function Negate(value0) {
          this.value0 = value0;
      };
      Negate.create = function (value0) {
          return new Negate(value0);
      };
      return Negate;
  })();
  var Apply = (function () {
      function Apply(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Apply.create = function (value0) {
          return function (value1) {
              return new Apply(value0, value1);
          };
      };
      return Apply;
  })();
  var BinOp = (function () {
      function BinOp(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      BinOp.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new BinOp(value0, value1, value2);
              };
          };
      };
      return BinOp;
  })();
  var Expression = (function () {
      function Expression(value0) {
          this.value0 = value0;
      };
      Expression.create = function (value0) {
          return new Expression(value0);
      };
      return Expression;
  })();
  var VariableAssignment = (function () {
      function VariableAssignment(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      VariableAssignment.create = function (value0) {
          return function (value1) {
              return new VariableAssignment(value0, value1);
          };
      };
      return VariableAssignment;
  })();
  var FunctionAssignment = (function () {
      function FunctionAssignment(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      FunctionAssignment.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new FunctionAssignment(value0, value1, value2);
              };
          };
      };
      return FunctionAssignment;
  })();
  var PrettyPrintFunction = (function () {
      function PrettyPrintFunction(value0) {
          this.value0 = value0;
      };
      PrettyPrintFunction.create = function (value0) {
          return new PrettyPrintFunction(value0);
      };
      return PrettyPrintFunction;
  })();
  var Command = (function () {
      function Command(value0) {
          this.value0 = value0;
      };
      Command.create = function (value0) {
          return new Command(value0);
      };
      return Command;
  })();
  exports["QConversionError"] = QConversionError;
  exports["WrongArityError"] = WrongArityError;
  exports["LookupError"] = LookupError;
  exports["NumericalError"] = NumericalError;
  exports["RedefinedConstantError"] = RedefinedConstantError;
  exports["InvalidIdentifier"] = InvalidIdentifier;
  exports["Add"] = Add;
  exports["Sub"] = Sub;
  exports["Mul"] = Mul;
  exports["Div"] = Div;
  exports["Pow"] = Pow;
  exports["Mod"] = Mod;
  exports["ConvertTo"] = ConvertTo;
  exports["Scalar"] = Scalar;
  exports["Unit"] = Unit;
  exports["Variable"] = Variable;
  exports["Factorial"] = Factorial;
  exports["Negate"] = Negate;
  exports["Apply"] = Apply;
  exports["BinOp"] = BinOp;
  exports["Help"] = Help;
  exports["Reset"] = Reset;
  exports["List"] = List;
  exports["Clear"] = Clear;
  exports["Copy"] = Copy;
  exports["Quit"] = Quit;
  exports["Expression"] = Expression;
  exports["VariableAssignment"] = VariableAssignment;
  exports["FunctionAssignment"] = FunctionAssignment;
  exports["PrettyPrintFunction"] = PrettyPrintFunction;
  exports["Command"] = Command;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Insect.Environment"] = $PS["Insect.Environment"] || {};
  var exports = $PS["Insect.Environment"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Quantity = $PS["Data.Quantity"];
  var Data_Quantity_Math = $PS["Data.Quantity.Math"];
  var Data_Quantity_Physics = $PS["Data.Quantity.Physics"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Insect_Functions = $PS["Insect.Functions"];
  var Insect_Language = $PS["Insect.Language"];                
  var Constant = (function () {
      function Constant() {

      };
      Constant.value = new Constant();
      return Constant;
  })();
  var HiddenConstant = (function () {
      function HiddenConstant() {

      };
      HiddenConstant.value = new HiddenConstant();
      return HiddenConstant;
  })();
  var UserDefined = (function () {
      function UserDefined() {

      };
      UserDefined.value = new UserDefined();
      return UserDefined;
  })();
  var StoredValue = (function () {
      function StoredValue(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      StoredValue.create = function (value0) {
          return function (value1) {
              return new StoredValue(value0, value1);
          };
      };
      return StoredValue;
  })();
  var BuiltinFunction = (function () {
      function BuiltinFunction(value0) {
          this.value0 = value0;
      };
      BuiltinFunction.create = function (value0) {
          return new BuiltinFunction(value0);
      };
      return BuiltinFunction;
  })();
  var UserFunction = (function () {
      function UserFunction(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      UserFunction.create = function (value0) {
          return function (value1) {
              return new UserFunction(value0, value1);
          };
      };
      return UserFunction;
  })();
  var StoredFunction = (function () {
      function StoredFunction(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      StoredFunction.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new StoredFunction(value0, value1, value2);
              };
          };
      };
      return StoredFunction;
  })();
  var initialEnvironment = (function () {
      var wrapSimple2 = function (name) {
          return function (func) {
              return function (qs) {
                  var numArgs = Data_List_NonEmpty.length(qs);
                  if (qs.value1 instanceof Data_List_Types.Cons && qs.value1.value1 instanceof Data_List_Types.Nil) {
                      return Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create)(func(qs.value0)(qs.value1.value0));
                  };
                  return Data_Either.Left.create(new Insect_Language.WrongArityError(name, 2, numArgs));
              };
          };
      };
      var wrapSimple = function (name) {
          return function (func) {
              return function (qs) {
                  var numArgs = Data_List_NonEmpty.length(qs);
                  var $7 = numArgs === 1;
                  if ($7) {
                      return Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create)(func(Data_List_NonEmpty.head(qs)));
                  };
                  return Data_Either.Left.create(new Insect_Language.WrongArityError(name, 1, numArgs));
              };
          };
      };
      var hiddenVal = function (identifier) {
          return function (value) {
              return new Data_Tuple.Tuple(identifier, new StoredValue(HiddenConstant.value, value));
          };
      };
      var constVal = function (identifier) {
          return function (value) {
              return new Data_Tuple.Tuple(identifier, new StoredValue(Constant.value, value));
          };
      };
      var constFuncN = function (identifier) {
          return function (func) {
              return new Data_Tuple.Tuple(identifier, new StoredFunction(Constant.value, func, new BuiltinFunction(Data_Maybe.Nothing.value)));
          };
      };
      var constFunc2 = function (identifier) {
          return function (func) {
              return new Data_Tuple.Tuple(identifier, new StoredFunction(Constant.value, wrapSimple2(identifier)(func), new BuiltinFunction(new Data_Maybe.Just(2))));
          };
      };
      var constFunc = function (identifier) {
          return function (func) {
              return new Data_Tuple.Tuple(identifier, new StoredFunction(Constant.value, wrapSimple(identifier)(func), new BuiltinFunction(new Data_Maybe.Just(1))));
          };
      };
      return {
          values: Data_Map_Internal.fromFoldable(Data_Ord.ordString)(Data_Foldable.foldableArray)([ constVal("alpha")(Data_Quantity_Physics.α), constVal("avogadroConstant")(Data_Quantity_Physics.avogadroConstant), constVal("bohrMagneton")(Data_Quantity_Physics.µB), constVal("boltzmannConstant")(Data_Quantity_Physics.kB), constVal("c")(Data_Quantity_Physics.speedOfLight), constVal("e")(Data_Quantity_Math.e), constVal("electricConstant")(Data_Quantity_Physics.ε0), constVal("eps0")(Data_Quantity_Physics.ε0), constVal("\u03b50")(Data_Quantity_Physics.ε0), constVal("elementaryCharge")(Data_Quantity_Physics.electronCharge), constVal("electronCharge")(Data_Quantity_Physics.electronCharge), constVal("electronMass")(Data_Quantity_Physics.electronMass), constVal("G")(Data_Quantity_Physics.gravitationalConstant), constVal("g0")(Data_Quantity_Physics.g0), constVal("gravity")(Data_Quantity_Physics.g0), constVal("h_bar")(Data_Quantity_Physics.ℏ), constVal("\u210f")(Data_Quantity_Physics.ℏ), constVal("k_B")(Data_Quantity_Physics.kB), constVal("magneticConstant")(Data_Quantity_Physics.µ0), constVal("mu0")(Data_Quantity_Physics.µ0), constVal("\xb50")(Data_Quantity_Physics.µ0), constVal("muB")(Data_Quantity_Physics.µB), constVal("\xb5_B")(Data_Quantity_Physics.µB), constVal("N_A")(Data_Quantity_Physics.avogadroConstant), constVal("pi")(Data_Quantity_Math.pi), constVal("\u03c0")(Data_Quantity_Math.pi), constVal("planckConstant")(Data_Quantity_Physics.planckConstant), constVal("protonMass")(Data_Quantity_Physics.protonMass), constVal("speedOfLight")(Data_Quantity_Physics.speedOfLight), constVal("R")(Data_Quantity_Physics.idealGasConstant), hiddenVal("hundred")(Data_Quantity.scalar(100.0)), hiddenVal("thousand")(Data_Quantity.scalar(1000.0)), hiddenVal("million")(Data_Quantity.scalar(1000000.0)), hiddenVal("billion")(Data_Quantity.scalar(1.0e9)), hiddenVal("trillion")(Data_Quantity.scalar(1.0e12)), hiddenVal("quadrillion")(Data_Quantity.scalar(1.0e15)), hiddenVal("quintillion")(Data_Quantity.scalar(1.0e18)), hiddenVal("googol")(Data_Quantity.scalar(1.0e100)), hiddenVal("tau")(Data_Quantity_Math.tau), hiddenVal("\u03c4")(Data_Quantity_Math.tau) ]),
          functions: Data_Map_Internal.fromFoldable(Data_Ord.ordString)(Data_Foldable.foldableArray)([ constFunc("abs")((function () {
              var $10 = Control_Applicative.pure(Data_Either.applicativeEither);
              return function ($11) {
                  return $10(Data_Quantity.abs($11));
              };
          })()), constFunc("acos")(Data_Quantity_Math.acos), constFunc("acosh")(Data_Quantity_Math.acosh), constFunc("acos")(Data_Quantity_Math.acos), constFunc("acosh")(Data_Quantity_Math.acosh), constFunc("asin")(Data_Quantity_Math.asin), constFunc("asinh")(Data_Quantity_Math.asinh), constFunc("atan")(Data_Quantity_Math.atan), constFunc2("atan2")(Data_Quantity_Math.atan2), constFunc("atanh")(Data_Quantity_Math.atanh), constFunc("ceil")(Data_Quantity_Math.ceil), constFunc("cos")(Data_Quantity_Math.cos), constFunc("cosh")(Data_Quantity_Math.cosh), constFunc("exp")(Data_Quantity_Math.exp), constFunc("floor")(Data_Quantity_Math.floor), constFunc("fromCelsius")(Insect_Functions.fromCelsius), constFunc("fromFahrenheit")(Insect_Functions.fromFahrenheit), constFunc("gamma")(Data_Quantity_Math.gamma), constFunc("ln")(Data_Quantity_Math.ln), constFunc("log")(Data_Quantity_Math.ln), constFunc("log10")(Data_Quantity_Math.log10), constFuncN("minimum")((function () {
              var $12 = Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create);
              return function ($13) {
                  return $12(Data_Quantity_Math.min(Data_List_Types.NonEmptyList($13)));
              };
          })()), constFuncN("maximum")((function () {
              var $14 = Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create);
              return function ($15) {
                  return $14(Data_Quantity_Math.max(Data_List_Types.NonEmptyList($15)));
              };
          })()), constFuncN("mean")((function () {
              var $16 = Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create);
              return function ($17) {
                  return $16(Data_Quantity_Math.mean(Data_List_Types.NonEmptyList($17)));
              };
          })()), constFunc("round")(Data_Quantity_Math.round), constFunc("sin")(Data_Quantity_Math.sin), constFunc("sinh")(Data_Quantity_Math.sinh), constFunc("sqrt")((function () {
              var $18 = Control_Applicative.pure(Data_Either.applicativeEither);
              return function ($19) {
                  return $18(Data_Quantity.sqrt($19));
              };
          })()), constFunc("tan")(Data_Quantity_Math.tan), constFunc("tanh")(Data_Quantity_Math.tanh), constFunc("toCelsius")(Insect_Functions.toCelsius), constFunc("toFahrenheit")(Insect_Functions.toFahrenheit) ])
      };
  })();
  var eqStorageType = new Data_Eq.Eq(function (x) {
      return function (y) {
          if (x instanceof Constant && y instanceof Constant) {
              return true;
          };
          if (x instanceof HiddenConstant && y instanceof HiddenConstant) {
              return true;
          };
          if (x instanceof UserDefined && y instanceof UserDefined) {
              return true;
          };
          return false;
      };
  });
  exports["Constant"] = Constant;
  exports["HiddenConstant"] = HiddenConstant;
  exports["UserDefined"] = UserDefined;
  exports["StoredValue"] = StoredValue;
  exports["BuiltinFunction"] = BuiltinFunction;
  exports["UserFunction"] = UserFunction;
  exports["StoredFunction"] = StoredFunction;
  exports["initialEnvironment"] = initialEnvironment;
  exports["eqStorageType"] = eqStorageType;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Insect.Format"] = $PS["Insect.Format"] || {};
  var exports = $PS["Insect.Format"];
  var Control_Category = $PS["Control.Category"];
  var Data_Array = $PS["Data.Array"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Monoid = $PS["Data.Monoid"];                
  var Normal = (function () {
      function Normal() {

      };
      Normal.value = new Normal();
      return Normal;
  })();
  var Optional = (function () {
      function Optional() {

      };
      Optional.value = new Optional();
      return Optional;
  })();
  var FTText = (function () {
      function FTText() {

      };
      FTText.value = new FTText();
      return FTText;
  })();
  var FTEmphasized = (function () {
      function FTEmphasized() {

      };
      FTEmphasized.value = new FTEmphasized();
      return FTEmphasized;
  })();
  var FTError = (function () {
      function FTError() {

      };
      FTError.value = new FTError();
      return FTError;
  })();
  var FTValue = (function () {
      function FTValue() {

      };
      FTValue.value = new FTValue();
      return FTValue;
  })();
  var FTIdentifier = (function () {
      function FTIdentifier() {

      };
      FTIdentifier.value = new FTIdentifier();
      return FTIdentifier;
  })();
  var FTFunction = (function () {
      function FTFunction() {

      };
      FTFunction.value = new FTFunction();
      return FTFunction;
  })();
  var FTUnit = (function () {
      function FTUnit() {

      };
      FTUnit.value = new FTUnit();
      return FTUnit;
  })();
  var Formatted = (function () {
      function Formatted(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      Formatted.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new Formatted(value0, value1, value2);
              };
          };
      };
      return Formatted;
  })();
  var val = Formatted.create(Normal.value)(FTValue.value);
  var unit = Formatted.create(Normal.value)(FTUnit.value);
  var uncurry = function (fmt) {
      return function (v) {
          return fmt(v.value0)(v.value1)(v.value2);
      };
  };
  var text = Formatted.create(Normal.value)(FTText.value);
  var optional = function (v) {
      return new Formatted(Optional.value, v.value1, v.value2);
  };
  var nl = text("\x0a");
  var jtClass = function (v) {
      return function (v1) {
          if (v1 === "") {
              return "";
          };
          return "[[;;;hl-" + (v + ("]" + (v1 + "]")));
      };
  };
  var ident = Formatted.create(Normal.value)(FTIdentifier.value);
  var $$function = Formatted.create(Normal.value)(FTFunction.value);
  var format = function (formatter) {
      return function (m) {
          return Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(uncurry(formatter))(Data_Array.cons(optional(nl))(m));
      };
  };
  var fmtPlain = function (v) {
      return function (v1) {
          return function (v2) {
              if (v instanceof Normal) {
                  return v2;
              };
              if (v instanceof Optional) {
                  return "";
              };
              throw new Error("Failed pattern match at Insect.Format (line 95, column 1 - line 95, column 21): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  var fmtJqueryTerminal = function (v) {
      return function (v1) {
          if (v1 instanceof FTText) {
              return Control_Category.identity(Control_Category.categoryFn);
          };
          if (v1 instanceof FTEmphasized) {
              return jtClass("emphasized");
          };
          if (v1 instanceof FTError) {
              return jtClass("error");
          };
          if (v1 instanceof FTValue) {
              return jtClass("value");
          };
          if (v1 instanceof FTIdentifier) {
              return jtClass("identifier");
          };
          if (v1 instanceof FTFunction) {
              return jtClass("function");
          };
          if (v1 instanceof FTUnit) {
              return jtClass("unit");
          };
          throw new Error("Failed pattern match at Insect.Format (line 104, column 1 - line 104, column 30): " + [ v.constructor.name, v1.constructor.name ]);
      };
  };
  var error = Formatted.create(Normal.value)(FTError.value);
  var emph = Formatted.create(Normal.value)(FTEmphasized.value);
  var consoleCode = function (code) {
      return function (str) {
          return "\x1b[" + (code + ("m" + (str + "\x1b[0m")));
      };
  };
  var fmtConsole = function (v) {
      return function (v1) {
          return function (s) {
              if (v1 instanceof FTText) {
                  return s;
              };
              if (v1 instanceof FTEmphasized) {
                  return consoleCode("01")(s);
              };
              if (v1 instanceof FTError) {
                  return consoleCode("31")(s);
              };
              if (v1 instanceof FTValue) {
                  return consoleCode("36")(s);
              };
              if (v1 instanceof FTIdentifier) {
                  return consoleCode("33")(s);
              };
              if (v1 instanceof FTFunction) {
                  return consoleCode("03")(s);
              };
              if (v1 instanceof FTUnit) {
                  return consoleCode("32")(s);
              };
              throw new Error("Failed pattern match at Insect.Format (line 117, column 1 - line 117, column 23): " + [ v.constructor.name, v1.constructor.name, s.constructor.name ]);
          };
      };
  };
  exports["text"] = text;
  exports["emph"] = emph;
  exports["error"] = error;
  exports["val"] = val;
  exports["ident"] = ident;
  exports["function"] = $$function;
  exports["unit"] = unit;
  exports["optional"] = optional;
  exports["nl"] = nl;
  exports["format"] = format;
  exports["fmtPlain"] = fmtPlain;
  exports["fmtJqueryTerminal"] = fmtJqueryTerminal;
  exports["fmtConsole"] = fmtConsole;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Insect.PrettyPrint"] = $PS["Insect.PrettyPrint"] || {};
  var exports = $PS["Insect.PrettyPrint"];
  var Data_Array = $PS["Data.Array"];
  var Data_Decimal = $PS["Data.Decimal"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Quantity = $PS["Data.Quantity"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Units = $PS["Data.Units"];
  var Insect_Format = $PS["Insect.Format"];
  var Insect_Language = $PS["Insect.Language"];                
  var prettyVariable = function (name) {
      return [ Insect_Format.ident(name) ];
  };
  var prettyUnit = function (u) {
      return [ Insect_Format.unit(Data_Units.toString(u)) ];
  };
  var prettyScalar = function (n) {
      return [ Insect_Format.val(Data_Decimal.toString(n)) ];
  };
  var prettyQuantity = function (q) {
      var rec = Data_Quantity["prettyPrint'"](q);
      var space = (function () {
          if (rec.space) {
              return " ";
          };
          return "";
      })();
      return [ Insect_Format.val(rec.number), Insect_Format.text(space), Insect_Format.unit(rec.unit) ];
  };
  var prettyQuantity$prime = function (s) {
      return function (u) {
          return prettyQuantity(Data_Quantity["quantity'"](s)(u));
      };
  };
  var prettyOp = function (op) {
      var opToStr = function (v) {
          if (v instanceof Insect_Language.Add) {
              return " + ";
          };
          if (v instanceof Insect_Language.Sub) {
              return " - ";
          };
          if (v instanceof Insect_Language.Mul) {
              return " \xd7 ";
          };
          if (v instanceof Insect_Language.Div) {
              return " / ";
          };
          if (v instanceof Insect_Language.Pow) {
              return "^";
          };
          if (v instanceof Insect_Language.Mod) {
              return " % ";
          };
          if (v instanceof Insect_Language.ConvertTo) {
              return " \u279e ";
          };
          throw new Error("Failed pattern match at Insect.PrettyPrint (line 25, column 5 - line 25, column 30): " + [ v.constructor.name ]);
      };
      return [ Insect_Format.text(opToStr(op)) ];
  };
  var parens = function (m) {
      return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.cons(Insect_Format.text("("))(m))([ Insect_Format.text(")") ]);
  };
  var withParens$prime = function (v) {
      if (v instanceof Insect_Language.Unit) {
          return pretty(v);
      };
      if (v instanceof Insect_Language.Scalar) {
          return pretty(v);
      };
      if (v instanceof Insect_Language.Variable) {
          return pretty(v);
      };
      if (v instanceof Insect_Language.Apply) {
          return pretty(v);
      };
      return parens(pretty(v));
  };
  var withParens = function (v) {
      if (v instanceof Insect_Language.BinOp && (v.value0 instanceof Insect_Language.Mul && (v.value1 instanceof Insect_Language.Scalar && v.value2 instanceof Insect_Language.Unit))) {
          return pretty(v);
      };
      return withParens$prime(v);
  };
  var prettyApply = function (fn) {
      return function (xs) {
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Insect_Format["function"](fn), Insect_Format.text("(") ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Foldable.intercalate(Data_NonEmpty.foldableNonEmpty(Data_List_Types.foldableList))(Data_Monoid.monoidArray)([ Insect_Format.text(", ") ])(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))(pretty)(xs)))([ Insect_Format.text(")") ]));
      };
  };
  var pretty = function (v) {
      if (v instanceof Insect_Language.Scalar) {
          return prettyScalar(v.value0);
      };
      if (v instanceof Insect_Language.Unit) {
          return prettyUnit(v.value0);
      };
      if (v instanceof Insect_Language.Variable) {
          return prettyVariable(v.value0);
      };
      if (v instanceof Insect_Language.Factorial) {
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)(withParens(v.value0))([ Insect_Format.text("!") ]);
      };
      if (v instanceof Insect_Language.Negate) {
          return Data_Array.cons(Insect_Format.text("-"))(withParens(v.value0));
      };
      if (v instanceof Insect_Language.Apply) {
          return prettyApply(v.value0)(v.value1);
      };
      if (v instanceof Insect_Language.BinOp && v.value0 instanceof Insect_Language.ConvertTo) {
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)(pretty(v.value1))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(prettyOp(Insect_Language.ConvertTo.value))(pretty(v.value2)));
      };
      if (v instanceof Insect_Language.BinOp && (v.value0 instanceof Insect_Language.Mul && (v.value1 instanceof Insect_Language.Scalar && v.value2 instanceof Insect_Language.Unit))) {
          return prettyQuantity$prime(v.value1.value0)(v.value2.value0);
      };
      if (v instanceof Insect_Language.BinOp && v.value0 instanceof Insect_Language.Mul) {
          var addP = function (ex) {
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Pow) {
                  return pretty(ex);
              };
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Mul) {
                  return pretty(ex);
              };
              return withParens(ex);
          };
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)(addP(v.value1))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(prettyOp(Insect_Language.Mul.value))(addP(v.value2)));
      };
      if (v instanceof Insect_Language.BinOp && v.value0 instanceof Insect_Language.Div) {
          var addPRight = function (ex) {
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Pow) {
                  return pretty(ex);
              };
              return withParens(ex);
          };
          var addPLeft = function (ex) {
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Pow) {
                  return pretty(ex);
              };
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Mul) {
                  return pretty(ex);
              };
              return withParens(ex);
          };
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)(addPLeft(v.value1))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(prettyOp(Insect_Language.Div.value))(addPRight(v.value2)));
      };
      if (v instanceof Insect_Language.BinOp && v.value0 instanceof Insect_Language.Add) {
          var addP = function (ex) {
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Pow) {
                  return pretty(ex);
              };
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Mul) {
                  return pretty(ex);
              };
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Add) {
                  return pretty(ex);
              };
              return withParens(ex);
          };
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)(addP(v.value1))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(prettyOp(Insect_Language.Add.value))(addP(v.value2)));
      };
      if (v instanceof Insect_Language.BinOp && v.value0 instanceof Insect_Language.Sub) {
          var addP = function (ex) {
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Pow) {
                  return pretty(ex);
              };
              if (ex instanceof Insect_Language.BinOp && ex.value0 instanceof Insect_Language.Mul) {
                  return pretty(ex);
              };
              return withParens(ex);
          };
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)(addP(v.value1))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(prettyOp(Insect_Language.Sub.value))(addP(v.value2)));
      };
      if (v instanceof Insect_Language.BinOp) {
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)(withParens$prime(v.value1))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(prettyOp(v.value0))(withParens$prime(v.value2)));
      };
      throw new Error("Failed pattern match at Insect.PrettyPrint (line 82, column 1 - line 82, column 29): " + [ v.constructor.name ]);
  };
  exports["pretty"] = pretty;
  exports["prettyQuantity"] = prettyQuantity;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Insect.Interpreter"] = $PS["Insect.Interpreter"] || {};
  var exports = $PS["Insect.Interpreter"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Array = $PS["Data.Array"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Int = $PS["Data.Int"];
  var Data_List = $PS["Data.List"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Quantity = $PS["Data.Quantity"];
  var Data_Quantity_Math = $PS["Data.Quantity.Math"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Show = $PS["Data.Show"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Units = $PS["Data.Units"];
  var Insect_Environment = $PS["Insect.Environment"];
  var Insect_Format = $PS["Insect.Format"];
  var Insect_Language = $PS["Insect.Language"];
  var Insect_PrettyPrint = $PS["Insect.PrettyPrint"];                
  var Value = (function () {
      function Value() {

      };
      Value.value = new Value();
      return Value;
  })();
  var ValueSet = (function () {
      function ValueSet() {

      };
      ValueSet.value = new ValueSet();
      return ValueSet;
  })();
  var Info = (function () {
      function Info() {

      };
      Info.value = new Info();
      return Info;
  })();
  var $$Error = (function () {
      function $$Error() {

      };
      $$Error.value = new $$Error();
      return $$Error;
  })();
  var Message = (function () {
      function Message(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Message.create = function (value0) {
          return function (value1) {
              return new Message(value0, value1);
          };
      };
      return Message;
  })();
  var MQuit = (function () {
      function MQuit() {

      };
      MQuit.value = new MQuit();
      return MQuit;
  })();
  var MCopy = (function () {
      function MCopy() {

      };
      MCopy.value = new MCopy();
      return MCopy;
  })();
  var MClear = (function () {
      function MClear() {

      };
      MClear.value = new MClear();
      return MClear;
  })();
  var prettyPrintFunction = function (name) {
      return function (argNames) {
          var fArgs = Data_Foldable.intercalate(Data_NonEmpty.foldableNonEmpty(Data_List_Types.foldableList))(Data_Monoid.monoidArray)([ Insect_Format.text(", ") ])(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))(function (a) {
              return [ Insect_Format.ident(a) ];
          })(argNames));
          return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Insect_Format["function"](name), Insect_Format.text("(") ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(fArgs)([ Insect_Format.text(") = ") ]));
      };
  };
  var isConstant = function (env) {
      return function (name) {
          var isConstantValue = (function () {
              var v = Data_Map_Internal.lookup(Data_Ord.ordString)(name)(env.values);
              if (v instanceof Data_Maybe.Just && v.value0.value0 instanceof Insect_Environment.Constant) {
                  return true;
              };
              if (v instanceof Data_Maybe.Just && v.value0.value0 instanceof Insect_Environment.HiddenConstant) {
                  return true;
              };
              return false;
          })();
          var isConstantFunction = (function () {
              var v = Data_Map_Internal.lookup(Data_Ord.ordString)(name)(env.functions);
              if (v instanceof Data_Maybe.Just && v.value0.value0 instanceof Insect_Environment.Constant) {
                  return true;
              };
              if (v instanceof Data_Maybe.Just && v.value0.value0 instanceof Insect_Environment.HiddenConstant) {
                  return true;
              };
              return false;
          })();
          return isConstantValue || isConstantFunction;
      };
  };
  var conversionErrorMessage = function (v) {
      var baseRep = function (u) {
          var us = Data_Units.baseRepresentation(u);
          var us$prime = Data_List.sortBy(Data_Ord.comparing(Data_Ord.ordString)(Data_Units.toString))(us);
          var usStrs = Data_Foldable.intercalate(Data_List_Types.foldableList)(Data_Monoid.monoidArray)([ Insect_Format.text("\xb7") ])(Data_Functor.map(Data_List_Types.functorList)(function ($161) {
              return Data_Array.singleton(Insect_Format.unit(Data_Units.toString($161)));
          })(us$prime));
          var br = Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.cons(Insect_Format.text(" (base units: "))(usStrs))([ Insect_Format.text(")") ]);
          var $44 = Data_Eq.eq(Data_Units.eqDerivedUnit)(Data_Tuple.fst(Data_Units.toStandardUnit(u)))(Data_Units.unity);
          if ($44) {
              return [  ];
          };
          var $45 = Data_Units.toString(u) === Insect_Format.format(Insect_Format.fmtPlain)(usStrs);
          if ($45) {
              return [  ];
          };
          return br;
      };
      var $46 = Data_Eq.eq(Data_Units.eqDerivedUnit)(v.value0)(Data_Units.unity);
      if ($46) {
          return [ Insect_Format.error("  Conversion error:"), Insect_Format.nl, Insect_Format.nl, Insect_Format.text("    Cannot convert a "), Insect_Format.unit("scalar"), Insect_Format.text(" to a quantity of unit "), Insect_Format.unit(Data_Units.toString(v.value1)) ];
      };
      var $47 = Data_Eq.eq(Data_Units.eqDerivedUnit)(v.value1)(Data_Units.unity);
      if ($47) {
          return [ Insect_Format.error("  Conversion error:"), Insect_Format.nl, Insect_Format.nl, Insect_Format.text("    Cannot convert quantity of unit "), Insect_Format.unit(Data_Units.toString(v.value0)), Insect_Format.text(" to a "), Insect_Format.unit("scalar") ];
      };
      return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Insect_Format.error("  Conversion error:"), Insect_Format.nl, Insect_Format.nl, Insect_Format.text("    Cannot convert unit "), Insect_Format.unit(Data_Units.toString(v.value0)) ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(baseRep(v.value0))(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Insect_Format.nl, Insect_Format.text("                to unit "), Insect_Format.unit(Data_Units.toString(v.value1)) ])(baseRep(v.value1))));
  };
  var evalErrorMessage = function (v) {
      if (v instanceof Insect_Language.QConversionError) {
          return conversionErrorMessage(v.value0);
      };
      if (v instanceof Insect_Language.WrongArityError) {
          return [ Insect_Format.optional(Insect_Format.text("  ")), Insect_Format.error("Wrong number of arguments:"), Insect_Format.nl, Insect_Format.nl, Insect_Format.text("    The function '"), Insect_Format["function"](v.value0), Insect_Format.text("'"), Insect_Format.text(" takes "), Insect_Format.val(Data_Show.show(Data_Show.showInt)(v.value1)), Insect_Format.text((function () {
              var $52 = v.value1 === 1;
              if ($52) {
                  return " argument";
              };
              return " arguments";
          })()), Insect_Format.text(" (got "), Insect_Format.val(Data_Show.show(Data_Show.showInt)(v.value2)), Insect_Format.text(")") ];
      };
      if (v instanceof Insect_Language.LookupError) {
          return [ Insect_Format.optional(Insect_Format.text("  ")), Insect_Format.error("Unknown identifier: "), Insect_Format.ident(v.value0) ];
      };
      if (v instanceof Insect_Language.NumericalError) {
          return [ Insect_Format.optional(Insect_Format.text("  ")), Insect_Format.error("Numerical error: "), Insect_Format.text("division by zero or out-of-bounds error") ];
      };
      if (v instanceof Insect_Language.RedefinedConstantError) {
          return [ Insect_Format.optional(Insect_Format.text("  ")), Insect_Format.error("Assignment error: "), Insect_Format.text("'"), Insect_Format.emph(v.value0), Insect_Format.text("' cannot be redefined.") ];
      };
      if (v instanceof Insect_Language.InvalidIdentifier) {
          return [ Insect_Format.optional(Insect_Format.text("  ")), Insect_Format.error("Invalid identifier: "), Insect_Format.text("second argument of '"), Insect_Format["function"](v.value0), Insect_Format.text("' must be a variable name.") ];
      };
      throw new Error("Failed pattern match at Insect.Interpreter (line 193, column 1 - line 193, column 38): " + [ v.constructor.name ]);
  };
  var errorWithInput = function (prefix) {
      return function (expr) {
          return function (env) {
              return function (err) {
                  return {
                      msg: Message.create($$Error.value)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(Insect_Format.optional)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.cons(Insect_Format.text("  "))(prefix))(Insect_PrettyPrint.pretty(expr))))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(Insect_Format.optional)([ Insect_Format.nl, Insect_Format.nl ]))(evalErrorMessage(err)))),
                      newEnv: env
                  };
              };
          };
      };
  };
  var checkFinite = function (q) {
      if (Data_Quantity["isFinite"](q)) {
          return Control_Applicative.pure(Data_Either.applicativeEither)(q);
      };
      if (Data_Boolean.otherwise) {
          return new Data_Either.Left(Insect_Language.NumericalError.value);
      };
      throw new Error("Failed pattern match at Insect.Interpreter (line 48, column 1 - line 48, column 41): " + [ q.constructor.name ]);
  };
  var evalSpecial = function (func) {
      return function (v) {
          return function (v1) {
              return function (v2) {
                  return function (v3) {
                      return function (v4) {
                          if (v2 instanceof Insect_Language.Variable) {
                              var qMultiply = function (q1$prime) {
                                  return function (q2$prime) {
                                      return Control_Bind.bind(Data_Either.bindEither)(q1$prime)(function (q1) {
                                          return Control_Bind.bind(Data_Either.bindEither)(q2$prime)(function (q2) {
                                              return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.qMultiply(q1)(q2));
                                          });
                                      });
                                  };
                              };
                              var qAdd = function (q1$prime) {
                                  return function (q2$prime) {
                                      return Control_Bind.bind(Data_Either.bindEither)(q1$prime)(function (q1) {
                                          return Control_Bind.bind(Data_Either.bindEither)(q2$prime)(function (q2) {
                                              return Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create)(Data_Quantity.qAdd(q1)(q2));
                                          });
                                      });
                                  };
                              };
                              return Control_Bind.bind(Data_Either.bindEither)($$eval(v)(v3))(function (lowQuantity) {
                                  return Control_Bind.bind(Data_Either.bindEither)($$eval(v)(v4))(function (highQuantity) {
                                      return Control_Bind.bind(Data_Either.bindEither)(Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create)(Data_Functor.map(Data_Either.functorEither)(Data_Int.round)(Data_Quantity.toScalar(lowQuantity))))(function (low) {
                                          return Control_Bind.bind(Data_Either.bindEither)(Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create)(Data_Functor.map(Data_Either.functorEither)(Data_Int.round)(Data_Quantity.toScalar(highQuantity))))(function (high) {
                                              var iteration = function (n) {
                                                  return $$eval({
                                                      values: Data_Map_Internal.insert(Data_Ord.ordString)(v2.value0)(new Insect_Environment.StoredValue(Insect_Environment.UserDefined.value, Data_Quantity.scalar(Data_Int.toNumber(n))))(v.values),
                                                      functions: v.functions
                                                  })(v1);
                                              };
                                              var qs = (function () {
                                                  var $66 = low > high;
                                                  if ($66) {
                                                      return Data_List_Types.Nil.value;
                                                  };
                                                  return Data_Functor.map(Data_List_Types.functorList)(iteration)(Data_List.range(low)(high));
                                              })();
                                              var $67 = func === "sum";
                                              if ($67) {
                                                  if (qs instanceof Data_List_Types.Cons) {
                                                      return Data_NonEmpty.foldl1(Data_List_Types.foldableList)(qAdd)(new Data_NonEmpty.NonEmpty(qs.value0, qs.value1));
                                                  };
                                                  if (qs instanceof Data_List_Types.Nil) {
                                                      return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.scalar(0.0));
                                                  };
                                                  throw new Error("Failed pattern match at Insect.Interpreter (line 83, column 7 - line 85, column 42): " + [ qs.constructor.name ]);
                                              };
                                              return Data_Foldable.foldl(Data_List_Types.foldableList)(qMultiply)(new Data_Either.Right(Data_Quantity.scalar(1.0)))(qs);
                                          });
                                      });
                                  });
                              });
                          };
                          return new Data_Either.Left(new Insect_Language.InvalidIdentifier(func));
                      };
                  };
              };
          };
      };
  };
  var $$eval = function (env) {
      return function (v) {
          if (v instanceof Insect_Language.Scalar) {
              return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity["scalar'"](v.value0));
          };
          if (v instanceof Insect_Language.Unit) {
              return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.quantity(1.0)(v.value0));
          };
          if (v instanceof Insect_Language.Variable) {
              var v1 = Data_Map_Internal.lookup(Data_Ord.ordString)(v.value0)(env.values);
              if (v1 instanceof Data_Maybe.Just) {
                  return Control_Applicative.pure(Data_Either.applicativeEither)(v1.value0.value1);
              };
              if (v1 instanceof Data_Maybe.Nothing) {
                  return new Data_Either.Left(new Insect_Language.LookupError(v.value0));
              };
              throw new Error("Failed pattern match at Insect.Interpreter (line 105, column 35 - line 107, column 70): " + [ v1.constructor.name ]);
          };
          if (v instanceof Insect_Language.Factorial) {
              return Control_Bind.bind(Data_Either.bindEither)($$eval(env)(v.value0))((function () {
                  var $162 = Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create);
                  return function ($163) {
                      return $162(Data_Quantity_Math.factorial($163));
                  };
              })());
          };
          if (v instanceof Insect_Language.Negate) {
              return Data_Functor.map(Data_Either.functorEither)(Data_Quantity.qNegate)($$eval(env)(v.value0));
          };
          if (v instanceof Insect_Language.Apply) {
              var $83 = v.value0 === "sum" || v.value0 === "product";
              if ($83) {
                  if (v.value1.value1 instanceof Data_List_Types.Cons && (v.value1.value1.value1 instanceof Data_List_Types.Cons && (v.value1.value1.value1.value1 instanceof Data_List_Types.Cons && v.value1.value1.value1.value1.value1 instanceof Data_List_Types.Nil))) {
                      return evalSpecial(v.value0)(env)(v.value1.value0)(v.value1.value1.value0)(v.value1.value1.value1.value0)(v.value1.value1.value1.value1.value0);
                  };
                  return new Data_Either.Left(new Insect_Language.WrongArityError(v.value0, 4, Data_List_NonEmpty.length(v.value1)));
              };
              var v1 = Data_Map_Internal.lookup(Data_Ord.ordString)(v.value0)(env.functions);
              if (v1 instanceof Data_Maybe.Just) {
                  return Control_Bind.bind(Data_Either.bindEither)(Control_Bind.bind(Data_Either.bindEither)(Data_Traversable.traverse(Data_NonEmpty.traversableNonEmpty(Data_List_Types.traversableList))(Data_Either.applicativeEither)($$eval(env))(v.value1))(v1.value0.value1))(checkFinite);
              };
              if (v1 instanceof Data_Maybe.Nothing) {
                  return new Data_Either.Left(new Insect_Language.LookupError(v.value0));
              };
              throw new Error("Failed pattern match at Insect.Interpreter (line 119, column 7 - line 122, column 42): " + [ v1.constructor.name ]);
          };
          if (v instanceof Insect_Language.BinOp) {
              var wrap = Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Insect_Language.QConversionError.create);
              var toScalar = function (q) {
                  return wrap(Data_Quantity["toScalar'"](q));
              };
              var qSubtract = function (q1) {
                  return function (q2) {
                      return wrap(Data_Quantity.qSubtract(q1)(q2));
                  };
              };
              var qAdd = function (q1) {
                  return function (q2) {
                      return wrap(Data_Quantity.qAdd(q1)(q2));
                  };
              };
              var modulo = function (q1) {
                  return function (q2) {
                      return wrap(Data_Quantity_Math.modulo(q1)(q2));
                  };
              };
              var convertTo = function (source) {
                  return function (target) {
                      return wrap(Data_Quantity.convertTo(source)(Data_Quantity.derivedUnit(target)));
                  };
              };
              var run = function (v1) {
                  return function (a) {
                      return function (b) {
                          if (v1 instanceof Insect_Language.Sub) {
                              return qSubtract(a)(b);
                          };
                          if (v1 instanceof Insect_Language.Add) {
                              return qAdd(a)(b);
                          };
                          if (v1 instanceof Insect_Language.Mul) {
                              return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.qMultiply(a)(b));
                          };
                          if (v1 instanceof Insect_Language.Div) {
                              return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Quantity.qDivide(a)(b));
                          };
                          if (v1 instanceof Insect_Language.Pow) {
                              return Data_Functor.map(Data_Either.functorEither)(Data_Quantity.pow(a))(toScalar(b));
                          };
                          if (v1 instanceof Insect_Language.Mod) {
                              return modulo(a)(b);
                          };
                          if (v1 instanceof Insect_Language.ConvertTo) {
                              return convertTo(a)(b);
                          };
                          throw new Error("Failed pattern match at Insect.Interpreter (line 128, column 5 - line 128, column 60): " + [ v1.constructor.name, a.constructor.name, b.constructor.name ]);
                      };
                  };
              };
              return Control_Bind.bind(Data_Either.bindEither)($$eval(env)(v.value1))(function (x$prime) {
                  return Control_Bind.bind(Data_Either.bindEither)($$eval(env)(v.value2))(function (y$prime) {
                      return Control_Bind.bind(Data_Either.bindEither)(run(v.value0)(x$prime)(y$prime))(checkFinite);
                  });
              });
          };
          throw new Error("Failed pattern match at Insect.Interpreter (line 102, column 1 - line 102, column 50): " + [ env.constructor.name, v.constructor.name ]);
      };
  };
  var evalAndSimplify = function (env) {
      return function (v) {
          if (v instanceof Insect_Language.BinOp && v.value0 instanceof Insect_Language.ConvertTo) {
              return $$eval(env)(v);
          };
          return Data_Functor.map(Data_Either.functorEither)(Data_Quantity.fullSimplify)($$eval(env)(v));
      };
  };
  var runInsect = function (env) {
      return function (v) {
          if (v instanceof Insect_Language.Expression) {
              var v1 = evalAndSimplify(env)(v.value0);
              if (v1 instanceof Data_Either.Left) {
                  return errorWithInput([  ])(v.value0)(env)(v1.value0);
              };
              if (v1 instanceof Data_Either.Right) {
                  return {
                      msg: Message.create(Value.value)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(Insect_Format.optional)(Data_Array.cons(Insect_Format.text("  "))(Insect_PrettyPrint.pretty(v.value0))))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(Insect_Format.optional)([ Insect_Format.nl, Insect_Format.nl, Insect_Format.text("   = ") ]))(Insect_PrettyPrint.prettyQuantity(v1.value0)))),
                      newEnv: {
                          values: Data_Map_Internal.insert(Data_Ord.ordString)("ans")(new Insect_Environment.StoredValue(Insect_Environment.UserDefined.value, v1.value0))(env.values),
                          functions: env.functions
                      }
                  };
              };
              throw new Error("Failed pattern match at Insect.Interpreter (line 259, column 3 - line 266, column 8): " + [ v1.constructor.name ]);
          };
          if (v instanceof Insect_Language.VariableAssignment) {
              var v1 = evalAndSimplify(env)(v.value1);
              if (v1 instanceof Data_Either.Left) {
                  return errorWithInput([ Insect_Format.ident(v.value0), Insect_Format.text(" = ") ])(v.value1)(env)(v1.value0);
              };
              if (v1 instanceof Data_Either.Right) {
                  var $119 = isConstant(env)(v.value0);
                  if ($119) {
                      return errorWithInput([ Insect_Format.ident(v.value0), Insect_Format.text(" = ") ])(v.value1)(env)(new Insect_Language.RedefinedConstantError(v.value0));
                  };
                  return {
                      msg: Message.create(ValueSet.value)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(Insect_Format.optional)([ Insect_Format.text("  "), Insect_Format.ident(v.value0), Insect_Format.text(" = ") ]))(Insect_PrettyPrint.prettyQuantity(v1.value0))),
                      newEnv: {
                          values: Data_Map_Internal.insert(Data_Ord.ordString)(v.value0)(new Insect_Environment.StoredValue(Insect_Environment.UserDefined.value, v1.value0))(env.values),
                          functions: Data_Map_Internal["delete"](Data_Ord.ordString)(v.value0)(env.functions)
                      }
                  };
              };
              throw new Error("Failed pattern match at Insect.Interpreter (line 269, column 3 - line 281, column 12): " + [ v1.constructor.name ]);
          };
          if (v instanceof Insect_Language.FunctionAssignment) {
              var numExpected = Data_List_NonEmpty.length(v.value1);
              var userFunc = function (argValues) {
                  var insertArg = function (map) {
                      return function (v1) {
                          return Data_Map_Internal.insert(Data_Ord.ordString)(v1.value0)(new Insect_Environment.StoredValue(Insect_Environment.UserDefined.value, v1.value1))(map);
                      };
                  };
                  var args = Data_List_NonEmpty.zip(v.value1)(argValues);
                  var functionEnv = {
                      values: Data_Foldable.foldl(Data_List_Types.foldableNonEmptyList)(insertArg)(env.values)(args),
                      functions: Data_Map_Internal["delete"](Data_Ord.ordString)(v.value0)(env.functions)
                  };
                  var numGiven = Data_List_NonEmpty.length(argValues);
                  var $127 = numGiven === numExpected;
                  if ($127) {
                      return evalAndSimplify(functionEnv)(v.value2);
                  };
                  return new Data_Either.Left(new Insect_Language.WrongArityError(v.value0, numExpected, numGiven));
              };
              var $128 = isConstant(env)(v.value0);
              if ($128) {
                  return errorWithInput(prettyPrintFunction(v.value0)(v.value1))(v.value2)(env)(new Insect_Language.RedefinedConstantError(v.value0));
              };
              return {
                  msg: Message.create(ValueSet.value)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(Insect_Format.optional)(Data_Array.cons(Insect_Format.text("  "))(prettyPrintFunction(v.value0)(v.value1))))(Insect_PrettyPrint.pretty(v.value2))),
                  newEnv: {
                      values: Data_Map_Internal["delete"](Data_Ord.ordString)(v.value0)(env.values),
                      functions: Data_Map_Internal.insert(Data_Ord.ordString)(v.value0)(new Insect_Environment.StoredFunction(Insect_Environment.UserDefined.value, userFunc, new Insect_Environment.UserFunction(v.value1, v.value2)))(env.functions)
                  }
              };
          };
          if (v instanceof Insect_Language.PrettyPrintFunction) {
              var message = (function () {
                  var v1 = Data_Map_Internal.lookup(Data_Ord.ordString)(v.value0)(env.functions);
                  if (v1 instanceof Data_Maybe.Just && v1.value0.value2 instanceof Insect_Environment.BuiltinFunction) {
                      var argText = (function () {
                          if (v1.value0.value2.value0 instanceof Data_Maybe.Just && v1.value0.value2.value0.value0 === 1) {
                              return "x";
                          };
                          if (v1.value0.value2.value0 instanceof Data_Maybe.Just && v1.value0.value2.value0.value0 === 2) {
                              return "x, y";
                          };
                          if (v1.value0.value2.value0 instanceof Data_Maybe.Just) {
                              return "x, y, \u2026";
                          };
                          if (v1.value0.value2.value0 instanceof Data_Maybe.Nothing) {
                              return "x1, x2, \u2026";
                          };
                          throw new Error("Failed pattern match at Insect.Interpreter (line 326, column 23 - line 330, column 47): " + [ v1.value0.value2.value0.constructor.name ]);
                      })();
                      return new Message(Info.value, [ Insect_Format.optional(Insect_Format.text("  ")), Insect_Format.ident(v.value0), Insect_Format.text("("), Insect_Format.text(argText), Insect_Format.text(") = builtin function") ]);
                  };
                  if (v1 instanceof Data_Maybe.Just && v1.value0.value2 instanceof Insect_Environment.UserFunction) {
                      return Message.create(Info.value)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(Insect_Format.optional)(Data_Array.cons(Insect_Format.text("  "))(prettyPrintFunction(v.value0)(v1.value0.value2.value0))))(Insect_PrettyPrint.pretty(v1.value0.value2.value1)));
                  };
                  if (v1 instanceof Data_Maybe.Nothing) {
                      return new Message($$Error.value, [ Insect_Format.text("Unknown function") ]);
                  };
                  throw new Error("Failed pattern match at Insect.Interpreter (line 318, column 7 - line 333, column 62): " + [ v1.constructor.name ]);
              })();
              return {
                  msg: message,
                  newEnv: env
              };
          };
          if (v instanceof Insect_Language.Command && v.value0 instanceof Insect_Language.Help) {
              return {
                  msg: new Message(Info.value, [ Insect_Format.emph("insect"), Insect_Format.text(" evaluates mathematical expressions that can"), Insect_Format.nl, Insect_Format.text("involve physical quantities. You can start by trying"), Insect_Format.nl, Insect_Format.text("one of these examples:"), Insect_Format.nl, Insect_Format.text(""), Insect_Format.nl, Insect_Format.emph("  > "), Insect_Format.val("1920"), Insect_Format.text(" / "), Insect_Format.val("16"), Insect_Format.text(" * "), Insect_Format.val("9"), Insect_Format.text("         "), Insect_Format.emph("  > "), Insect_Format["function"]("sin"), Insect_Format.text("("), Insect_Format.val("30"), Insect_Format.text(" "), Insect_Format.unit("deg"), Insect_Format.text(")"), Insect_Format.nl, Insect_Format.text(""), Insect_Format.nl, Insect_Format.emph("  > "), Insect_Format.val("2"), Insect_Format.text(" "), Insect_Format.unit("min"), Insect_Format.text(" + "), Insect_Format.val("30"), Insect_Format.text(" "), Insect_Format.unit("s"), Insect_Format.text("          "), Insect_Format.emph("  > "), Insect_Format.val("6"), Insect_Format.text(" "), Insect_Format.unit("Mbit/s"), Insect_Format.text(" * "), Insect_Format.val("1.5"), Insect_Format.text(" "), Insect_Format.unit("h"), Insect_Format.text(" -> "), Insect_Format.unit("GB"), Insect_Format.nl, Insect_Format.text(""), Insect_Format.nl, Insect_Format.emph("  > "), Insect_Format.text("list"), Insect_Format.text("                  "), Insect_Format.emph("  > "), Insect_Format.ident("r"), Insect_Format.text(" = "), Insect_Format.val("80"), Insect_Format.text(" "), Insect_Format.unit("cm"), Insect_Format.nl, Insect_Format.emph("  > "), Insect_Format.val("40000"), Insect_Format.text(" "), Insect_Format.unit("km"), Insect_Format.text(" / "), Insect_Format.ident("c"), Insect_Format.text(" -> "), Insect_Format.unit("ms"), Insect_Format.text("    "), Insect_Format.emph("  > "), Insect_Format.ident("pi"), Insect_Format.text(" * "), Insect_Format.ident("r"), Insect_Format.text("^"), Insect_Format.val("2"), Insect_Format.text(" -> "), Insect_Format.unit("m"), Insect_Format.text("^"), Insect_Format.val("2"), Insect_Format.nl, Insect_Format.text(""), Insect_Format.nl, Insect_Format.text("Full documentation: https://github.com/sharkdp/insect") ]),
                  newEnv: env
              };
          };
          if (v instanceof Insect_Language.Command && v.value0 instanceof Insect_Language.List) {
              var storedValue = function (v1) {
                  return v1.value1;
              };
              var toLine = function (kvPairs) {
                  var val = storedValue(Data_Tuple.snd(Data_List_NonEmpty.head(kvPairs)));
                  var identifiers = Data_Array.fromFoldable(Data_Foldable.foldableArray)(Data_Foldable.intercalate(Data_List_Types.foldableNonEmptyList)(Data_Monoid.monoidArray)([ Insect_Format.text(" = ") ])(Data_Functor.map(Data_List_Types.functorNonEmptyList)(function ($164) {
                      return Data_Array.singleton(Insect_Format.ident(Data_Tuple.fst($164)));
                  })(kvPairs)));
                  return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Insect_Format.nl, Insect_Format.text("  ") ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(identifiers)(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Insect_Format.text(" = ") ])(Insect_PrettyPrint.prettyQuantity(val))));
              };
              var storageType = function (v1) {
                  return v1.value0;
              };
              var visibleValues = Data_List.filter(function (e) {
                  return Data_Eq.notEq(Insect_Environment.eqStorageType)(storageType(Data_Tuple.snd(e)))(Insect_Environment.HiddenConstant.value);
              })(Data_Map_Internal.toUnfoldable(Data_List_Types.unfoldableList)(env.values));
              var envTuples = Data_List.sortBy(Data_Ord.comparing(Data_Ord.ordString)(function ($165) {
                  return (function (v1) {
                      return v1.number;
                  })(Data_Quantity["prettyPrint'"](storedValue(Data_Tuple.snd($165))));
              }))(visibleValues);
              var envGrouped = Data_List.groupBy(function (x) {
                  return function (y) {
                      return Data_Eq.eq(Data_Quantity.eqQuantity)(storedValue(Data_Tuple.snd(x)))(storedValue(Data_Tuple.snd(y)));
                  };
              })(envTuples);
              var envSorted = Data_List.sortBy(Data_Ord.comparing(Data_Ord.ordString)(function ($166) {
                  return Data_String_Common.toLower(Data_Tuple.fst(Data_List_NonEmpty.head($166)));
              }))(envGrouped);
              var list = Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Insect_Format.text("List of variables:"), Insect_Format.nl ])(Data_Foldable.foldMap(Data_List_Types.foldableList)(Data_Monoid.monoidArray)(toLine)(envSorted));
              return {
                  msg: new Message(Info.value, list),
                  newEnv: env
              };
          };
          if (v instanceof Insect_Language.Command && v.value0 instanceof Insect_Language.Reset) {
              return {
                  msg: new Message(Info.value, [ Insect_Format.text("Environment has been reset.") ]),
                  newEnv: Insect_Environment.initialEnvironment
              };
          };
          if (v instanceof Insect_Language.Command && v.value0 instanceof Insect_Language.Quit) {
              return {
                  msg: MQuit.value,
                  newEnv: Insect_Environment.initialEnvironment
              };
          };
          if (v instanceof Insect_Language.Command && v.value0 instanceof Insect_Language.Copy) {
              return {
                  msg: MCopy.value,
                  newEnv: env
              };
          };
          if (v instanceof Insect_Language.Command && v.value0 instanceof Insect_Language.Clear) {
              return {
                  msg: MClear.value,
                  newEnv: env
              };
          };
          throw new Error("Failed pattern match at Insect.Interpreter (line 257, column 1 - line 257, column 47): " + [ env.constructor.name, v.constructor.name ]);
      };
  };
  exports["Value"] = Value;
  exports["ValueSet"] = ValueSet;
  exports["Info"] = Info;
  exports["Error"] = $$Error;
  exports["Message"] = Message;
  exports["MQuit"] = MQuit;
  exports["MCopy"] = MCopy;
  exports["MClear"] = MClear;
  exports["runInsect"] = runInsect;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Text.Parsing.Parser.Pos"] = $PS["Text.Parsing.Parser.Pos"] || {};
  var exports = $PS["Text.Parsing.Parser.Pos"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_String_Pattern = $PS["Data.String.Pattern"];
  var updatePosString = function (pos$prime) {
      return function (str) {
          var updatePosChar = function (v) {
              return function (c) {
                  if (c === "\x0a") {
                      return {
                          line: v.line + 1 | 0,
                          column: 1
                      };
                  };
                  if (c === "\x0d") {
                      return {
                          line: v.line + 1 | 0,
                          column: 1
                      };
                  };
                  if (c === "\x09") {
                      return {
                          line: v.line,
                          column: (v.column + 8 | 0) - Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(v.column - 1 | 0)(8) | 0
                      };
                  };
                  return {
                      line: v.line,
                      column: v.column + 1 | 0
                  };
              };
          };
          return Data_Foldable.foldl(Data_Foldable.foldableArray)(updatePosChar)(pos$prime)(Data_String_Common.split(Data_Newtype.wrap(Data_String_Pattern.newtypePattern)(""))(str));
      };
  }; 
  var initialPos = {
      line: 1,
      column: 1
  };
  exports["initialPos"] = initialPos;
  exports["updatePosString"] = updatePosString;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Text.Parsing.Parser"] = $PS["Text.Parsing.Parser"] || {};
  var exports = $PS["Text.Parsing.Parser"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Alternative = $PS["Control.Alternative"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_State_Trans = $PS["Control.Monad.State.Trans"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Either = $PS["Data.Either"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Text_Parsing_Parser_Pos = $PS["Text.Parsing.Parser.Pos"];                
  var ParseState = (function () {
      function ParseState(value0, value1, value2) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
      };
      ParseState.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return new ParseState(value0, value1, value2);
              };
          };
      };
      return ParseState;
  })();
  var ParseError = (function () {
      function ParseError(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      ParseError.create = function (value0) {
          return function (value1) {
              return new ParseError(value0, value1);
          };
      };
      return ParseError;
  })();
  var ParserT = function (x) {
      return x;
  }; 
  var parseErrorPosition = function (v) {
      return v.value1;
  };
  var parseErrorMessage = function (v) {
      return v.value0;
  };
  var newtypeParserT = new Data_Newtype.Newtype(function (n) {
      return n;
  }, ParserT);
  var runParserT = function (dictMonad) {
      return function (s) {
          return function (p) {
              var initialState = new ParseState(s, Text_Parsing_Parser_Pos.initialPos, false);
              return Control_Monad_State_Trans.evalStateT(((dictMonad.Bind1()).Apply0()).Functor0())(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(newtypeParserT)(p)))(initialState);
          };
      };
  };
  var runParser = function (s) {
      var $90 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
      var $91 = runParserT(Data_Identity.monadIdentity)(s);
      return function ($92) {
          return $90($91($92));
      };
  }; 
  var monadThrowParserT = function (dictMonad) {
      return Control_Monad_Except_Trans.monadThrowExceptT(Control_Monad_State_Trans.monadStateT(dictMonad));
  };
  var monadStateParserT = function (dictMonad) {
      return Control_Monad_Except_Trans.monadStateExceptT(Control_Monad_State_Trans.monadStateStateT(dictMonad));
  };
  var position = function (dictMonad) {
      return Control_Monad_State_Class.gets(monadStateParserT(dictMonad))(function (v) {
          return v.value1;
      });
  };   
  var lazyParserT = new Control_Lazy.Lazy(function (f) {
      return Control_Lazy.defer(Control_Monad_State_Trans.lazyStateT)((function () {
          var $98 = Data_Newtype.unwrap(newtypeParserT);
          return function ($99) {
              return Control_Monad_Except_Trans.runExceptT($98(f($99)));
          };
      })());
  });                           
  var functorParserT = function (dictFunctor) {
      return Control_Monad_Except_Trans.functorExceptT(Control_Monad_State_Trans.functorStateT(dictFunctor));
  };
  var failWithPosition = function (dictMonad) {
      return function (message) {
          return function (pos) {
              return Control_Monad_Error_Class.throwError(monadThrowParserT(dictMonad))(new ParseError(message, pos));
          };
      };
  };
  var bindParserT = function (dictMonad) {
      return Control_Monad_Except_Trans.bindExceptT(Control_Monad_State_Trans.monadStateT(dictMonad));
  };
  var fail = function (dictMonad) {
      return function (message) {
          return Control_Bind.bindFlipped(bindParserT(dictMonad))(failWithPosition(dictMonad)(message))(position(dictMonad));
      };
  };
  var applyParserT = function (dictMonad) {
      return Control_Monad_Except_Trans.applyExceptT(Control_Monad_State_Trans.monadStateT(dictMonad));
  };
  var applicativeParserT = function (dictMonad) {
      return Control_Monad_Except_Trans.applicativeExceptT(Control_Monad_State_Trans.monadStateT(dictMonad));
  };
  var altParserT = function (dictMonad) {
      return new Control_Alt.Alt(function () {
          return functorParserT(((dictMonad.Bind1()).Apply0()).Functor0());
      }, function (p1) {
          return function (p2) {
              return ParserT(Control_Monad_Except_Trans.ExceptT(Control_Monad_State_Trans.StateT(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_State_Trans.runStateT(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(newtypeParserT)(p1)))(new ParseState(v.value0, v.value1, false)))(function (v1) {
                      if (v1.value0 instanceof Data_Either.Left && !v1.value1.value2) {
                          return Control_Monad_State_Trans.runStateT(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(newtypeParserT)(p2)))(v);
                      };
                      return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, v1.value1));
                  });
              })));
          };
      });
  };
  var plusParserT = function (dictMonad) {
      return new Control_Plus.Plus(function () {
          return altParserT(dictMonad);
      }, fail(dictMonad)("No alternative"));
  };
  var alternativeParserT = function (dictMonad) {
      return new Control_Alternative.Alternative(function () {
          return applicativeParserT(dictMonad);
      }, function () {
          return plusParserT(dictMonad);
      });
  };
  exports["ParseError"] = ParseError;
  exports["parseErrorMessage"] = parseErrorMessage;
  exports["parseErrorPosition"] = parseErrorPosition;
  exports["ParseState"] = ParseState;
  exports["ParserT"] = ParserT;
  exports["runParser"] = runParser;
  exports["fail"] = fail;
  exports["newtypeParserT"] = newtypeParserT;
  exports["lazyParserT"] = lazyParserT;
  exports["functorParserT"] = functorParserT;
  exports["applyParserT"] = applyParserT;
  exports["applicativeParserT"] = applicativeParserT;
  exports["bindParserT"] = bindParserT;
  exports["monadStateParserT"] = monadStateParserT;
  exports["altParserT"] = altParserT;
  exports["plusParserT"] = plusParserT;
  exports["alternativeParserT"] = alternativeParserT;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Text.Parsing.Parser.Combinators"] = $PS["Text.Parsing.Parser.Combinators"] || {};
  var exports = $PS["Text.Parsing.Parser.Combinators"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_State_Trans = $PS["Control.Monad.State.Trans"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];                
  var withErrorMessage = function (dictMonad) {
      return function (p) {
          return function (msg) {
              return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(p)(Text_Parsing_Parser.fail(dictMonad)("Expected " + msg));
          };
      };
  };
  var tryRethrow = function (dictMonad) {
      return function (p) {
          return Text_Parsing_Parser.ParserT(Control_Monad_Except_Trans.ExceptT(Control_Monad_State_Trans.StateT(function (v) {
              return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_State_Trans.runStateT(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(Text_Parsing_Parser.newtypeParserT)(p)))(v))(function (v1) {
                  if (v1.value0 instanceof Data_Either.Left) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(new Data_Either.Left(new Text_Parsing_Parser.ParseError(v1.value0.value0.value0, v.value1)), new Text_Parsing_Parser.ParseState(v1.value1.value0, v1.value1.value1, v.value2)));
                  };
                  return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, v1.value1));
              });
          })));
      };
  };
  var $$try = function (dictMonad) {
      return function (p) {
          return Text_Parsing_Parser.ParserT(Control_Monad_Except_Trans.ExceptT(Control_Monad_State_Trans.StateT(function (v) {
              return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_State_Trans.runStateT(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(Text_Parsing_Parser.newtypeParserT)(p)))(v))(function (v1) {
                  if (v1.value0 instanceof Data_Either.Left) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, new Text_Parsing_Parser.ParseState(v1.value1.value0, v1.value1.value1, v.value2)));
                  };
                  return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, v1.value1));
              });
          })));
      };
  };
  var skipMany1 = function (dictMonad) {
      return function (p) {
          return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(p)(function (x) {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(skipMany(dictMonad)(p))(function (xs) {
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_Unit.unit);
              });
          });
      };
  };
  var skipMany = function (dictMonad) {
      return function (p) {
          return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(skipMany1(dictMonad)(p))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_Unit.unit));
      };
  };
  var sepBy1 = function (dictMonad) {
      return function (p) {
          return function (sep) {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(p)(function (a) {
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_List.many(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(sep)(p)))(function (as) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(new Data_List_Types.Cons(a, as));
                  });
              });
          };
      };
  };
  var sepBy = function (dictMonad) {
      return function (p) {
          return function (sep) {
              return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(sepBy1(dictMonad)(p)(sep))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_List_Types.Nil.value));
          };
      };
  };
  var option = function (dictMonad) {
      return function (a) {
          return function (p) {
              return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(p)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(a));
          };
      };
  };
  var optionMaybe = function (dictMonad) {
      return function (p) {
          return option(dictMonad)(Data_Maybe.Nothing.value)(Data_Functor.map(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Data_Maybe.Just.create)(p));
      };
  };
  var notFollowedBy = function (dictMonad) {
      return function (p) {
          return $$try(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))($$try(dictMonad)(p))(Text_Parsing_Parser.fail(dictMonad)("Negated parser succeeded")))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_Unit.unit)));
      };
  };
  var choice = function (dictFoldable) {
      return function (dictMonad) {
          return Data_Foldable.foldl(dictFoldable)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad)))(Control_Plus.empty(Text_Parsing_Parser.plusParserT(dictMonad)));
      };
  };
  var between = function (dictMonad) {
      return function (open) {
          return function (close) {
              return function (p) {
                  return Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(dictMonad))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(open)(p))(close);
              };
          };
      };
  };
  var asErrorMessage = function (dictMonad) {
      return Data_Function.flip(withErrorMessage(dictMonad));
  };
  exports["withErrorMessage"] = withErrorMessage;
  exports["asErrorMessage"] = asErrorMessage;
  exports["between"] = between;
  exports["option"] = option;
  exports["optionMaybe"] = optionMaybe;
  exports["try"] = $$try;
  exports["tryRethrow"] = tryRethrow;
  exports["sepBy"] = sepBy;
  exports["sepBy1"] = sepBy1;
  exports["choice"] = choice;
  exports["skipMany"] = skipMany;
  exports["skipMany1"] = skipMany1;
  exports["notFollowedBy"] = notFollowedBy;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Text.Parsing.Parser.String"] = $PS["Text.Parsing.Parser.String"] || {};
  var exports = $PS["Text.Parsing.Parser.String"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Show = $PS["Data.Show"];
  var Data_String_CodePoints = $PS["Data.String.CodePoints"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_String_Pattern = $PS["Data.String.Pattern"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];
  var Text_Parsing_Parser_Combinators = $PS["Text.Parsing.Parser.Combinators"];
  var Text_Parsing_Parser_Pos = $PS["Text.Parsing.Parser.Pos"];                
  var StringLike = function (drop, indexOf, $$null, uncons) {
      this.drop = drop;
      this.indexOf = indexOf;
      this["null"] = $$null;
      this.uncons = uncons;
  };
  var uncons = function (dict) {
      return dict.uncons;
  };
  var stringLikeString = new StringLike(Data_String_CodePoints.drop, Data_String_CodePoints.indexOf, Data_String_Common["null"], Data_String_CodeUnits.uncons);
  var $$null = function (dict) {
      return dict["null"];
  };
  var indexOf = function (dict) {
      return dict.indexOf;
  };
  var eof = function (dictStringLike) {
      return function (dictMonad) {
          return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.gets(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v) {
              return v.value0;
          }))(function (input) {
              return Control_Applicative.unless(Text_Parsing_Parser.applicativeParserT(dictMonad))($$null(dictStringLike)(input))(Text_Parsing_Parser.fail(dictMonad)("Expected EOF"));
          });
      };
  };
  var drop = function (dict) {
      return dict.drop;
  };
  var string = function (dictStringLike) {
      return function (dictMonad) {
          return function (str) {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.gets(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v) {
                  return v.value0;
              }))(function (input) {
                  var v = indexOf(dictStringLike)(Data_Newtype.wrap(Data_String_Pattern.newtypePattern)(str))(input);
                  if (v instanceof Data_Maybe.Just && v.value0 === 0) {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.modify_(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v1) {
                          return new Text_Parsing_Parser.ParseState(drop(dictStringLike)(Data_String_CodePoints.length(str))(input), Text_Parsing_Parser_Pos.updatePosString(v1.value1)(str), true);
                      }))(function () {
                          return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(str);
                      });
                  };
                  return Text_Parsing_Parser.fail(dictMonad)("Expected " + Data_Show.show(Data_Show.showString)(str));
              });
          };
      };
  };
  var anyChar = function (dictStringLike) {
      return function (dictMonad) {
          return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.gets(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v) {
              return v.value0;
          }))(function (input) {
              var v = uncons(dictStringLike)(input);
              if (v instanceof Data_Maybe.Nothing) {
                  return Text_Parsing_Parser.fail(dictMonad)("Unexpected EOF");
              };
              if (v instanceof Data_Maybe.Just) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.modify_(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v1) {
                      return new Text_Parsing_Parser.ParseState(v.value0.tail, Text_Parsing_Parser_Pos.updatePosString(v1.value1)(Data_String_CodeUnits.singleton(v.value0.head)), true);
                  }))(function () {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v.value0.head);
                  });
              };
              throw new Error("Failed pattern match at Text.Parsing.Parser.String (line 56, column 3 - line 63, column 16): " + [ v.constructor.name ]);
          });
      };
  };
  var satisfy = function (dictStringLike) {
      return function (dictMonad) {
          return function (f) {
              return Text_Parsing_Parser_Combinators.tryRethrow(dictMonad)(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(anyChar(dictStringLike)(dictMonad))(function (c) {
                  var $52 = f(c);
                  if ($52) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(c);
                  };
                  return Text_Parsing_Parser.fail(dictMonad)("Character '" + (Data_String_CodeUnits.singleton(c) + "' did not satisfy predicate"));
              }));
          };
      };
  };
  var $$char = function (dictStringLike) {
      return function (dictMonad) {
          return function (c) {
              return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(satisfy(dictStringLike)(dictMonad)(function (v) {
                  return v === c;
              }))(Data_Show.show(Data_Show.showChar)(c));
          };
      };
  };
  var noneOf = function (dictStringLike) {
      return function (dictMonad) {
          return function (ss) {
              return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(satisfy(dictStringLike)(dictMonad)(Data_Function.flip(Data_Foldable.notElem(Data_Foldable.foldableArray)(Data_Eq.eqChar))(ss)))("none of " + Data_Show.show(Data_Show.showArray(Data_Show.showChar))(ss));
          };
      };
  };
  var oneOf = function (dictStringLike) {
      return function (dictMonad) {
          return function (ss) {
              return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(satisfy(dictStringLike)(dictMonad)(Data_Function.flip(Data_Foldable.elem(Data_Foldable.foldableArray)(Data_Eq.eqChar))(ss)))("one of " + Data_Show.show(Data_Show.showArray(Data_Show.showChar))(ss));
          };
      };
  };
  exports["eof"] = eof;
  exports["string"] = string;
  exports["satisfy"] = satisfy;
  exports["char"] = $$char;
  exports["oneOf"] = oneOf;
  exports["noneOf"] = noneOf;
  exports["stringLikeString"] = stringLikeString;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Text.Parsing.Parser.Token"] = $PS["Text.Parsing.Parser.Token"] || {};
  var exports = $PS["Text.Parsing.Parser.Token"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Char = $PS["Data.Char"];
  var Data_Char_Unicode = $PS["Data.Char.Unicode"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Int = $PS["Data.Int"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Show = $PS["Data.Show"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];
  var Data_Unit = $PS["Data.Unit"];
  var $$Math = $PS["Math"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];
  var Text_Parsing_Parser_Combinators = $PS["Text.Parsing.Parser.Combinators"];
  var Text_Parsing_Parser_String = $PS["Text.Parsing.Parser.String"];
  var upper = function (dictMonad) {
      return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.isUpper))("uppercase letter");
  };
  var theReservedNames = function (dictMonad) {
      return function (v) {
          if (v.caseSensitive) {
              return Data_Array.sort(Data_Ord.ordString)(v.reservedNames);
          };
          if (Data_Boolean.otherwise) {
              return Data_Array.sort(Data_Ord.ordString)(Data_Functor.map(Data_Functor.functorArray)(Data_String_Common.toLower)(v.reservedNames));
          };
          throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 722, column 1 - line 722, column 82): " + [ v.constructor.name ]);
      };
  };
  var space = function (dictMonad) {
      return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.isSpace))("space");
  };
  var simpleSpace = function (dictMonad) {
      return Text_Parsing_Parser_Combinators.skipMany1(dictMonad)(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.isSpace));
  };
  var oneLineComment = function (dictMonad) {
      return function (v) {
          return Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_Combinators["try"](dictMonad)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(v.commentLine)))(Text_Parsing_Parser_Combinators.skipMany(dictMonad)(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(function (v1) {
              return v1 !== "\x0a";
          })));
      };
  };
  var octDigit = function (dictMonad) {
      return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.isOctDigit))("oct digit");
  };
  var letter = function (dictMonad) {
      return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.isAlpha))("letter");
  };
  var isReserved = function ($copy_names) {
      return function ($copy_name) {
          var $tco_var_names = $copy_names;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(names, name) {
              var v = Data_Array.uncons(names);
              if (v instanceof Data_Maybe.Nothing) {
                  $tco_done = true;
                  return false;
              };
              if (v instanceof Data_Maybe.Just) {
                  var v1 = Data_Ord.compare(Data_Ord.ordString)(v.value0.head)(name);
                  if (v1 instanceof Data_Ordering.LT) {
                      $tco_var_names = v.value0.tail;
                      $copy_name = name;
                      return;
                  };
                  if (v1 instanceof Data_Ordering.EQ) {
                      $tco_done = true;
                      return true;
                  };
                  if (v1 instanceof Data_Ordering.GT) {
                      $tco_done = true;
                      return false;
                  };
                  throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 717, column 39 - line 720, column 53): " + [ v1.constructor.name ]);
              };
              throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 715, column 5 - line 720, column 53): " + [ v.constructor.name ]);
          };
          while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_names, $copy_name);
          };
          return $tco_result;
      };
  };
  var isReservedName = function (dictMonad) {
      return function (v) {
          return function (name) {
              var caseName = (function () {
                  if (v.caseSensitive) {
                      return name;
                  };
                  if (Data_Boolean.otherwise) {
                      return Data_String_Common.toLower(name);
                  };
                  throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 710, column 5 - line 711, column 57): " + [  ]);
              })();
              return isReserved(theReservedNames(dictMonad)(v))(caseName);
          };
      };
  };
  var inCommentSingle = function (dictMonad) {
      return function (v) {
          var startEnd = Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_String_CodeUnits.toCharArray(v.commentEnd))(Data_String_CodeUnits.toCharArray(v.commentStart));
          return Control_Lazy.fix(Text_Parsing_Parser.lazyParserT)(function (p) {
              return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Data_Functor["void"](Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Text_Parsing_Parser_Combinators["try"](dictMonad)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(v.commentEnd))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_Combinators.skipMany1(dictMonad)(Text_Parsing_Parser_String.noneOf(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(startEnd)))(p)))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(startEnd))(p)))("end of comment");
          });
      };
  };
  var multiLineComment = function (dictMonad) {
      return function (v) {
          return Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_Combinators["try"](dictMonad)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(v.commentStart)))(inComment(dictMonad)(v));
      };
  };
  var inCommentMulti = function (dictMonad) {
      return function (v) {
          var startEnd = Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_String_CodeUnits.toCharArray(v.commentEnd))(Data_String_CodeUnits.toCharArray(v.commentStart));
          return Control_Lazy.fix(Text_Parsing_Parser.lazyParserT)(function (p) {
              return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Data_Functor["void"](Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Text_Parsing_Parser_Combinators["try"](dictMonad)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(v.commentEnd))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(multiLineComment(dictMonad)(v))(p)))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_Combinators.skipMany1(dictMonad)(Text_Parsing_Parser_String.noneOf(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(startEnd)))(p)))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(startEnd))(p)))("end of comment");
          });
      };
  };
  var inComment = function (dictMonad) {
      return function (v) {
          if (v.nestedComments) {
              return inCommentMulti(dictMonad)(v);
          };
          return inCommentSingle(dictMonad)(v);
      };
  };
  var whiteSpace$prime = function (dictMonad) {
      return function (v) {
          if (Data_String_Common["null"](v.commentLine) && Data_String_Common["null"](v.commentStart)) {
              return Text_Parsing_Parser_Combinators.skipMany(dictMonad)(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(simpleSpace(dictMonad))(""));
          };
          if (Data_String_Common["null"](v.commentLine)) {
              return Text_Parsing_Parser_Combinators.skipMany(dictMonad)(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(simpleSpace(dictMonad))(multiLineComment(dictMonad)(v)))(""));
          };
          if (Data_String_Common["null"](v.commentStart)) {
              return Text_Parsing_Parser_Combinators.skipMany(dictMonad)(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(simpleSpace(dictMonad))(oneLineComment(dictMonad)(v)))(""));
          };
          if (Data_Boolean.otherwise) {
              return Text_Parsing_Parser_Combinators.skipMany(dictMonad)(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(simpleSpace(dictMonad))(oneLineComment(dictMonad)(v)))(multiLineComment(dictMonad)(v)))(""));
          };
          throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 731, column 1 - line 731, column 86): " + [ v.constructor.name ]);
      };
  };
  var hexDigit = function (dictMonad) {
      return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.isHexDigit))("hex digit");
  };
  var digit = function (dictMonad) {
      return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.isDigit))("digit");
  };
  var makeTokenParser = function (dictMonad) {
      return function (v) {
          var stringLetter = Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(function (c) {
              return c !== "\"" && (c !== "\\" && c > "\x1a");
          });
          var sign = function (dictRing) {
              return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("-"))(Data_Ring.negate(dictRing)))(Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("+"))(Control_Category.identity(Control_Category.categoryFn))))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Control_Category.identity(Control_Category.categoryFn)));
          };
          var oper = (function () {
              var go = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(v.opStart)(function (c) {
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Array.many(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(v.opLetter))(function (cs) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_String_CodeUnits.singleton(c) + Data_String_CodeUnits.fromCharArray(cs));
                  });
              });
              return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(go)("operator");
          })();
          var number = function (base) {
              return function (baseDigit) {
                  var folder = function (v1) {
                      return function (v2) {
                          if (v1 instanceof Data_Maybe.Nothing) {
                              return Data_Maybe.Nothing.value;
                          };
                          if (v1 instanceof Data_Maybe.Just) {
                              return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                                  return (base * v1.value0 | 0) + v3 | 0;
                              })(Data_Char_Unicode.digitToInt(v2));
                          };
                          throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 600, column 9 - line 600, column 49): " + [ v1.constructor.name, v2.constructor.name ]);
                      };
                  };
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Array.some(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(baseDigit))(function (digits) {
                      return Data_Maybe.maybe(Text_Parsing_Parser.fail(dictMonad)("not digits"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad)))(Data_Foldable.foldl(Data_Foldable.foldableArray)(folder)(new Data_Maybe.Just(0))(digits));
                  });
              };
          };
          var octal = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(dictMonad)([ "o", "O" ]))(number(8)(octDigit(dictMonad)));
          var lexeme = function (p) {
              return Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(dictMonad))(p)(whiteSpace$prime(dictMonad)(v));
          };
          var reservedOp = function (name) {
              var go = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(name))(function () {
                  return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_Combinators.notFollowedBy(dictMonad)(v.opLetter))("end of " + name);
              });
              return lexeme(Text_Parsing_Parser_Combinators["try"](dictMonad)(go));
          };
          var symbol = function (name) {
              return Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(lexeme(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(name)))(name);
          };
          var parens = function (p) {
              return Text_Parsing_Parser_Combinators.between(dictMonad)(symbol("("))(symbol(")"))(p);
          };
          var semi = symbol(";");
          var semiSep = function (p) {
              return Text_Parsing_Parser_Combinators.sepBy(dictMonad)(p)(semi);
          };
          var semiSep1 = function (p) {
              return Text_Parsing_Parser_Combinators.sepBy1(dictMonad)(p)(semi);
          };
          var isReservedOp = function (name) {
              return isReserved(Data_Array.sort(Data_Ord.ordString)(v.reservedOpNames))(name);
          };
          var operator = (function () {
              var go = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(oper)(function (name) {
                  var $82 = isReservedOp(name);
                  if ($82) {
                      return Text_Parsing_Parser.fail(dictMonad)("reserved operator " + name);
                  };
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(name);
              });
              return lexeme(Text_Parsing_Parser_Combinators["try"](dictMonad)(go));
          })();
          var ident = (function () {
              var go = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(v.identStart)(function (c) {
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Array.many(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(v.identLetter))(function (cs) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_String_CodeUnits.singleton(c) + Data_String_CodeUnits.fromCharArray(cs));
                  });
              });
              return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(go)("identifier");
          })();
          var identifier = (function () {
              var go = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(ident)(function (name) {
                  var $83 = isReservedName(dictMonad)(v)(name);
                  if ($83) {
                      return Text_Parsing_Parser.fail(dictMonad)("reserved word " + Data_Show.show(Data_Show.showString)(name));
                  };
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(name);
              });
              return lexeme(Text_Parsing_Parser_Combinators["try"](dictMonad)(go));
          })();
          var hexadecimal = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(dictMonad)([ "x", "X" ]))(number(16)(hexDigit(dictMonad)));
          var fraction = (function () {
              var op = function (v1) {
                  return function (v2) {
                      if (v2 instanceof Data_Maybe.Nothing) {
                          return Data_Maybe.Nothing.value;
                      };
                      if (v2 instanceof Data_Maybe.Just) {
                          return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Char_Unicode.digitToInt(v1))(function (int$prime) {
                              return Control_Applicative.pure(Data_Maybe.applicativeMaybe)((v2.value0 + Data_Int.toNumber(int$prime)) / 10.0);
                          });
                      };
                      throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 550, column 9 - line 550, column 51): " + [ v1.constructor.name, v2.constructor.name ]);
                  };
              };
              return Text_Parsing_Parser_Combinators.asErrorMessage(dictMonad)("fraction")(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("."))(function () {
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Data_Array.some(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(digit(dictMonad)))("fraction"))(function (digits) {
                      return Data_Maybe.maybe(Text_Parsing_Parser.fail(dictMonad)("not digit"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad)))(Data_Foldable.foldr(Data_Foldable.foldableArray)(op)(new Data_Maybe.Just(0.0))(digits));
                  });
              }));
          })();
          var escapeGap = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Data_Array.some(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(space(dictMonad)))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("\\")))("end of string gap");
          var escapeEmpty = Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("&");
          var escMap = Data_Array.zip([ "a", "b", "f", "n", "r", "t", "v", "\\", "\"", "'" ])([ "\x07", "\x08", "\x0c", "\x0a", "\x0d", "\x09", "\x0b", "\\", "\"", "'" ]);
          var dot = symbol(".");
          var decimal = number(10)(digit(dictMonad));
          var exponent$prime = (function () {
              var power = function (e) {
                  if (e < 0) {
                      return 1.0 / power(-e | 0);
                  };
                  if (Data_Boolean.otherwise) {
                      return $$Math.pow(10.0)(Data_Int.toNumber(e));
                  };
                  throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 563, column 9 - line 563, column 31): " + [ e.constructor.name ]);
              };
              return Text_Parsing_Parser_Combinators.asErrorMessage(dictMonad)("exponent")(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(dictMonad)([ "e", "E" ]))(function () {
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(sign(Data_Ring.ringInt))(function (f) {
                      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(decimal)("exponent"))(function (e) {
                          return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(power(f(e)));
                      });
                  });
              }));
          })();
          var fractExponent = function (n) {
              var justExponent = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(exponent$prime)(function (expo) {
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_Int.toNumber(n) * expo);
              });
              var fractExponent$prime = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(fraction)(function (fract) {
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_Combinators.option(dictMonad)(1.0)(exponent$prime))(function (expo) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))((Data_Int.toNumber(n) + fract) * expo);
                  });
              });
              return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(fractExponent$prime)(justExponent);
          };
          var fractFloat = function (n) {
              return Data_Functor.map(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Data_Either.Right.create)(fractExponent(n));
          };
          var decimalFloat = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(decimal)(function (n) {
              return Text_Parsing_Parser_Combinators.option(dictMonad)(new Data_Either.Left(n))(fractFloat(n));
          });
          var zeroNumFloat = Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Data_Functor.map(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Data_Either.Left.create)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(hexadecimal)(octal)))(decimalFloat))(fractFloat(0)))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(new Data_Either.Left(0)));
          var natFloat = Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("0"))(zeroNumFloat))(decimalFloat);
          var naturalOrFloat = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(lexeme(natFloat))("number");
          var floating = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(decimal)(fractExponent);
          var $$float = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(lexeme(floating))("float");
          var zeroNumber = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("0"))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(hexadecimal)(octal))(decimal))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(0))))("");
          var nat = Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(zeroNumber)(decimal);
          var $$int = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(lexeme(sign(Data_Ring.ringInt)))(function (f) {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(nat)(function (n) {
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(f(n));
              });
          });
          var integer = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(lexeme($$int))("integer");
          var natural = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(lexeme(nat))("natural");
          var comma = symbol(",");
          var commaSep = function (p) {
              return Text_Parsing_Parser_Combinators.sepBy(dictMonad)(p)(comma);
          };
          var commaSep1 = function (p) {
              return Text_Parsing_Parser_Combinators.sepBy1(dictMonad)(p)(comma);
          };
          var colon = symbol(":");
          var charNum = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(decimal)(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("o"))(number(8)(octDigit(dictMonad)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("x"))(number(16)(hexDigit(dictMonad)))))(function (code) {
              var $88 = code > 1114111;
              if ($88) {
                  return Text_Parsing_Parser.fail(dictMonad)("invalid escape sequence");
              };
              var v1 = Data_Char.fromCharCode(code);
              if (v1 instanceof Data_Maybe.Just) {
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v1.value0);
              };
              if (v1 instanceof Data_Maybe.Nothing) {
                  return Text_Parsing_Parser.fail(dictMonad)("invalid character code (should not happen)");
              };
              throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 448, column 17 - line 450, column 81): " + [ v1.constructor.name ]);
          });
          var charLetter = Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(function (c) {
              return c !== "'" && (c !== "\\" && c > "\x1a");
          });
          var charEsc = (function () {
              var parseEsc = function (v1) {
                  return Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)(v1.value0))(v1.value1);
              };
              return Text_Parsing_Parser_Combinators.choice(Data_Foldable.foldableArray)(dictMonad)(Data_Functor.map(Data_Functor.functorArray)(parseEsc)(escMap));
          })();
          var charControl = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("^"))(function () {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(upper(dictMonad))(function (code) {
                  var v1 = Data_Char.fromCharCode((Data_Char.toCharCode(code) - Data_Char.toCharCode("A") | 0) + 1 | 0);
                  if (v1 instanceof Data_Maybe.Just) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v1.value0);
                  };
                  if (v1 instanceof Data_Maybe.Nothing) {
                      return Text_Parsing_Parser.fail(dictMonad)("invalid character code (should not happen)");
                  };
                  throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 437, column 9 - line 439, column 73): " + [ v1.constructor.name ]);
              });
          });
          var caseString = function (name) {
              if (v.caseSensitive) {
                  return Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(name))(name);
              };
              if (Data_Boolean.otherwise) {
                  var msg = Data_Show.show(Data_Show.showString)(name);
                  var caseChar = function (c) {
                      if (Data_Char_Unicode.isAlpha(c)) {
                          return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.toLower(c)))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)(Data_Char_Unicode.toUpper(c)));
                      };
                      if (Data_Boolean.otherwise) {
                          return Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)(c);
                      };
                      throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 658, column 9 - line 658, column 50): " + [ c.constructor.name ]);
                  };
                  var walk = function (name$prime) {
                      var v1 = Data_String_CodeUnits.uncons(name$prime);
                      if (v1 instanceof Data_Maybe.Nothing) {
                          return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_Unit.unit);
                      };
                      if (v1 instanceof Data_Maybe.Just) {
                          return Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(caseChar(v1.value0.head))(msg))(walk(v1.value0.tail));
                      };
                      throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 654, column 22 - line 656, column 86): " + [ v1.constructor.name ]);
                  };
                  return Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(walk(name))(name);
              };
              throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 649, column 5 - line 649, column 52): " + [ name.constructor.name ]);
          };
          var reserved = function (name) {
              var go = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(caseString(name))(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_Combinators.notFollowedBy(dictMonad)(v.identLetter))("end of " + name));
              return lexeme(Text_Parsing_Parser_Combinators["try"](dictMonad)(go));
          };
          var brackets = function (p) {
              return Text_Parsing_Parser_Combinators.between(dictMonad)(symbol("["))(symbol("]"))(p);
          };
          var braces = function (p) {
              return Text_Parsing_Parser_Combinators.between(dictMonad)(symbol("{"))(symbol("}"))(p);
          };
          var ascii3codes = [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL" ];
          var ascii3 = [ "\x00", "\x01", "\x02", "\x03", "\x04", "\x05", "\x06", "\x07", "\x10", "\x11", "\x12", "\x13", "\x14", "\x15", "\x16", "\x17", "\x18", "\x1a", "\x1b", "\x7f" ];
          var ascii2codes = [ "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP" ];
          var ascii2 = [ "\x08", "\x09", "\x0a", "\x0b", "\x0c", "\x0d", "\x0e", "\x0f", "\x19", "\x1c", "\x1d", "\x1e", "\x1f", " " ];
          var asciiMap = Data_Array.zip(Data_Semigroup.append(Data_Semigroup.semigroupArray)(ascii3codes)(ascii2codes))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(ascii3)(ascii2));
          var charAscii = (function () {
              var parseAscii = function (v1) {
                  return Text_Parsing_Parser_Combinators["try"](dictMonad)(Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(dictMonad)(v1.value0))(v1.value1));
              };
              return Text_Parsing_Parser_Combinators.choice(Data_Foldable.foldableArray)(dictMonad)(Data_Functor.map(Data_Functor.functorArray)(parseAscii)(asciiMap));
          })();
          var escapeCode = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(charEsc)(charNum))(charAscii))(charControl))("escape code");
          var charEscape = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("\\"))(escapeCode);
          var characterChar = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(charLetter)(charEscape))("literal character");
          var charLiteral = (function () {
              var go = Text_Parsing_Parser_Combinators.between(dictMonad)(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("'"))(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("'"))("end of character"))(characterChar);
              return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(lexeme(go))("character");
          })();
          var stringEscape = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("\\"))(function () {
              return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(escapeGap)(Data_Maybe.Nothing.value))(Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(escapeEmpty)(Data_Maybe.Nothing.value)))(Data_Functor.map(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Data_Maybe.Just.create)(escapeCode));
          });
          var stringChar = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Data_Functor.map(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Data_Maybe.Just.create)(stringLetter))(stringEscape))("string character");
          var stringLiteral = (function () {
              var folder = function (v1) {
                  return function (chars) {
                      if (v1 instanceof Data_Maybe.Nothing) {
                          return chars;
                      };
                      if (v1 instanceof Data_Maybe.Just) {
                          return new Data_List_Types.Cons(v1.value0, chars);
                      };
                      throw new Error("Failed pattern match at Text.Parsing.Parser.Token (line 404, column 9 - line 404, column 55): " + [ v1.constructor.name, chars.constructor.name ]);
                  };
              };
              var go = Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_Combinators.between(dictMonad)(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("\""))(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(dictMonad)("\""))("end of string"))(Data_List.many(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(stringChar)))(function (maybeChars) {
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_String_CodeUnits.fromCharArray(Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray)(Data_Foldable.foldr(Data_List_Types.foldableList)(folder)(Data_List_Types.Nil.value)(maybeChars))));
              });
              return lexeme(Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(go)("literal string"));
          })();
          var angles = function (p) {
              return Text_Parsing_Parser_Combinators.between(dictMonad)(symbol("<"))(symbol(">"))(p);
          };
          return {
              identifier: identifier,
              reserved: reserved,
              operator: operator,
              reservedOp: reservedOp,
              charLiteral: charLiteral,
              stringLiteral: stringLiteral,
              natural: natural,
              integer: integer,
              "float": $$float,
              naturalOrFloat: naturalOrFloat,
              decimal: decimal,
              hexadecimal: hexadecimal,
              octal: octal,
              symbol: symbol,
              lexeme: lexeme,
              whiteSpace: whiteSpace$prime(dictMonad)(v),
              parens: parens,
              braces: braces,
              angles: angles,
              brackets: brackets,
              semi: semi,
              comma: comma,
              colon: colon,
              dot: dot,
              semiSep: semiSep,
              semiSep1: semiSep1,
              commaSep: commaSep,
              commaSep1: commaSep1
          };
      };
  };
  exports["makeTokenParser"] = makeTokenParser;
  exports["digit"] = digit;
  exports["letter"] = letter;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Insect.Parser"] = $PS["Insect.Parser"] || {};
  var exports = $PS["Insect.Parser"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Decimal = $PS["Data.Decimal"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_String_CodePoints = $PS["Data.String.CodePoints"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Units = $PS["Data.Units"];
  var Data_Units_Astronomical = $PS["Data.Units.Astronomical"];
  var Data_Units_Bit = $PS["Data.Units.Bit"];
  var Data_Units_CGS = $PS["Data.Units.CGS"];
  var Data_Units_Currency = $PS["Data.Units.Currency"];
  var Data_Units_Imperial = $PS["Data.Units.Imperial"];
  var Data_Units_Misc = $PS["Data.Units.Misc"];
  var Data_Units_Nautical = $PS["Data.Units.Nautical"];
  var Data_Units_PartsPerX = $PS["Data.Units.PartsPerX"];
  var Data_Units_SI = $PS["Data.Units.SI"];
  var Data_Units_SI_Accepted = $PS["Data.Units.SI.Accepted"];
  var Data_Units_SI_Derived = $PS["Data.Units.SI.Derived"];
  var Data_Units_Time = $PS["Data.Units.Time"];
  var Data_Units_USCustomary = $PS["Data.Units.USCustomary"];
  var Insect_Language = $PS["Insect.Language"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];
  var Text_Parsing_Parser_Combinators = $PS["Text.Parsing.Parser.Combinators"];
  var Text_Parsing_Parser_String = $PS["Text.Parsing.Parser.String"];
  var Text_Parsing_Parser_Token = $PS["Text.Parsing.Parser.Token"];                
  var DictEntry = (function () {
      function DictEntry(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      DictEntry.create = function (value0) {
          return function (value1) {
              return new DictEntry(value0, value1);
          };
      };
      return DictEntry;
  })();
  var Dictionary = (function () {
      function Dictionary(value0) {
          this.value0 = value0;
      };
      Dictionary.create = function (value0) {
          return new Dictionary(value0);
      };
      return Dictionary;
  })();
  var specialCases = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("d"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Data_Units_Time.day)))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("t"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Data_Units_SI_Accepted.tonne)));
  var sepBy1 = function (dictMonad) {
      return function (p) {
          return function (sep) {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(p)(function (a) {
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_List.many(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(sep)(p)))(function (as) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(new Data_NonEmpty.NonEmpty(a, as));
                  });
              });
          };
      };
  };
  var prefixDict = new Dictionary([ new DictEntry(Data_Units.kibi, [ "kibi", "Ki" ]), new DictEntry(Data_Units.mebi, [ "mebi", "Mi" ]), new DictEntry(Data_Units.gibi, [ "gibi", "Gi" ]), new DictEntry(Data_Units.tebi, [ "tebi", "Ti" ]), new DictEntry(Data_Units.pebi, [ "pebi", "Pi" ]), new DictEntry(Data_Units.exbi, [ "exbi", "Ei" ]), new DictEntry(Data_Units.zebi, [ "zebi", "Zi" ]), new DictEntry(Data_Units.yobi, [ "yobi", "Yi" ]), new DictEntry(Data_Units.atto, [ "atto", "a" ]), new DictEntry(Data_Units.femto, [ "femto", "f" ]), new DictEntry(Data_Units.peta, [ "peta" ]), new DictEntry(Data_Units.mega, [ "mega" ]), new DictEntry(Data_Units.pico, [ "pico", "p" ]), new DictEntry(Data_Units.nano, [ "nano", "n" ]), new DictEntry(Data_Units.micro, [ "micro", "u", "\xb5", "\u03bc" ]), new DictEntry(Data_Units.milli, [ "milli", "m" ]), new DictEntry(Data_Units.centi, [ "centi", "c" ]), new DictEntry(Data_Units.deci, [ "deci", "d" ]), new DictEntry(Data_Units.hecto, [ "hecto", "h" ]), new DictEntry(Data_Units.kilo, [ "kilo", "k" ]), new DictEntry(Data_Units.mega, [ "M" ]), new DictEntry(Data_Units.giga, [ "giga", "G" ]), new DictEntry(Data_Units.tera, [ "tera", "T" ]), new DictEntry(Data_Units.peta, [ "P" ]), new DictEntry(Data_Units.exa, [ "exa", "E" ]) ]);
  var normalUnitDict = new Dictionary([ new DictEntry(Data_Units_SI_Derived.radian, [ "radians", "radian", "rad" ]), new DictEntry(Data_Units_SI_Accepted.degree, [ "degrees", "degree", "deg", "\xb0" ]), new DictEntry(Data_Units_SI_Derived.hertz, [ "hertz", "Hz" ]), new DictEntry(Data_Units_Misc.rpm, [ "RPM", "rpm" ]), new DictEntry(Data_Units_SI_Derived.newton, [ "newton", "N" ]), new DictEntry(Data_Units_SI_Derived.joule, [ "joules", "joule", "J" ]), new DictEntry(Data_Units_SI_Derived.pascal, [ "pascal", "Pa" ]), new DictEntry(Data_Units_SI_Derived.volt, [ "volts", "volt", "V" ]), new DictEntry(Data_Units_SI_Derived.farad, [ "farad", "F" ]), new DictEntry(Data_Units_SI_Derived.ohm, [ "ohms", "ohm", "\u03a9" ]), new DictEntry(Data_Units_SI_Derived.sievert, [ "sievert", "Sv" ]), new DictEntry(Data_Units_SI_Derived.weber, [ "weber", "Wb" ]), new DictEntry(Data_Units_SI_Derived.tesla, [ "tesla", "T" ]), new DictEntry(Data_Units_SI_Derived.henry, [ "henrys", "henries", "henry", "H" ]), new DictEntry(Data_Units_SI_Derived.coulomb, [ "coulomb", "C" ]), new DictEntry(Data_Units_SI_Derived.siemens, [ "siemens", "S" ]), new DictEntry(Data_Units_SI_Derived.lumen, [ "lumen", "lm" ]), new DictEntry(Data_Units_SI_Derived.lux, [ "lux", "lx" ]), new DictEntry(Data_Units_SI_Derived.becquerel, [ "becquerel", "Bq" ]), new DictEntry(Data_Units_SI_Derived.gray, [ "gray", "Gy" ]), new DictEntry(Data_Units_SI_Derived.katal, [ "katal", "kat" ]), new DictEntry(Data_Units_SI_Accepted.hectare, [ "hectare", "ha" ]), new DictEntry(Data_Units_SI_Accepted.tonne, [ "tonnes", "tonne", "tons", "ton" ]), new DictEntry(Data_Units_SI_Accepted.electronvolt, [ "electronvolt", "eV" ]), new DictEntry(Data_Units_Misc.calorie, [ "calories", "calorie", "cal" ]), new DictEntry(Data_Units_SI_Accepted.bel, [ "bel" ]), new DictEntry(Data_Units_SI_Accepted.astronomicalUnit, [ "AU", "au", "astronomicalunits", "astronomicalunit" ]), new DictEntry(Data_Units_Astronomical.parsec, [ "parsecs", "parsec", "pc" ]), new DictEntry(Data_Units_Astronomical.lightyear, [ "lightyears", "lightyear", "ly" ]), new DictEntry(Data_Units_SI_Accepted.barn, [ "barn" ]), new DictEntry(Data_Units_SI_Accepted.bar, [ "bar" ]), new DictEntry(Data_Units_SI_Accepted.angstrom, [ "angstrom", "\xc5" ]), new DictEntry(Data_Units_CGS.gauss, [ "gauss" ]), new DictEntry(Data_Units_SI.ampere, [ "amperes", "ampere", "amps", "amp", "A" ]), new DictEntry(Data_Units_SI.mole, [ "mole", "mol" ]), new DictEntry(Data_Units_SI.kelvin, [ "kelvin", "K" ]), new DictEntry(Data_Units_SI.candela, [ "candela", "cd" ]), new DictEntry(Data_Semigroup.append(Data_Units.semigroupDerivedUnit)(Data_Units_SI_Derived.watt)(Data_Units_Time.hour), [ "Wh" ]), new DictEntry(Data_Units_SI_Derived.watt, [ "watts", "watt", "W" ]), new DictEntry(Data_Units_Bit["byte"], [ "Bytes", "bytes", "Byte", "byte", "B", "Octets", "octets", "Octet", "octet" ]), new DictEntry(Data_Units_Bit.bit, [ "bits", "bit" ]), new DictEntry(Data_Units.divideUnits(Data_Units_Bit.bit)(Data_Units_SI.second), [ "bps" ]), new DictEntry(Data_Units_SI.second, [ "seconds", "second", "sec", "s" ]), new DictEntry(Data_Units_Time.minute, [ "minutes", "minute", "min" ]), new DictEntry(Data_Units_Time.hour, [ "hours", "hour", "h" ]), new DictEntry(Data_Units_Time.day, [ "days", "day" ]), new DictEntry(Data_Units_Time.week, [ "weeks", "week" ]), new DictEntry(Data_Units_Misc.fortnight, [ "fortnights", "fortnight" ]), new DictEntry(Data_Units_Time.month, [ "months", "month" ]), new DictEntry(Data_Units_Time.year, [ "years", "year" ]), new DictEntry(Data_Units_SI.gram, [ "grammes", "gramme", "grams", "gram", "g" ]), new DictEntry(Data_Units_SI.meter, [ "metres", "metre", "meters", "meter", "m" ]), new DictEntry(Data_Units_SI_Accepted.liter, [ "liters", "liter", "litres", "litre", "L", "l" ]), new DictEntry(Data_Units_Misc.atm, [ "atm" ]), new DictEntry(Data_Units_Misc.pixel, [ "pixels", "pixel", "px" ]), new DictEntry(Data_Units_Misc.frame, [ "frames", "frame" ]), new DictEntry(Data_Units.divideUnits(Data_Units_Misc.frame)(Data_Units_SI.second), [ "fps" ]), new DictEntry(Data_Units_Misc.dot, [ "dots", "dot" ]) ]);
  var imperialUnitDict = new Dictionary([ new DictEntry(Data_Units_PartsPerX.percent, [ "pct", "percent" ]), new DictEntry(Data_Units_PartsPerX.partsPerMillion, [ "ppm" ]), new DictEntry(Data_Units_PartsPerX.partsPerBillion, [ "ppb" ]), new DictEntry(Data_Units_PartsPerX.partsPerTrillion, [ "ppt" ]), new DictEntry(Data_Units_PartsPerX.partsPerQuadrillion, [ "ppq" ]), new DictEntry(Data_Units_Imperial.mile, [ "miles", "mile" ]), new DictEntry(Data_Units.divideUnits(Data_Units_Imperial.mile)(Data_Units_Time.hour), [ "mph" ]), new DictEntry(Data_Units_Imperial.inch, [ "inches", "inch", "in" ]), new DictEntry(Data_Units_Imperial.yard, [ "yards", "yard", "yd" ]), new DictEntry(Data_Units_Imperial.foot, [ "feet", "foot", "ft" ]), new DictEntry(Data_Units_Imperial.thou, [ "thou", "mils", "mil" ]), new DictEntry(Data_Units_Imperial.ounce, [ "ounces", "ounce", "oz" ]), new DictEntry(Data_Units_Misc.lbf, [ "pound_force", "lbf" ]), new DictEntry(Data_Units_Imperial.pound, [ "pounds", "pound", "lb" ]), new DictEntry(Data_Units_USCustomary.gallon, [ "gallons", "gallon", "gal" ]), new DictEntry(Data_Units_USCustomary.pint, [ "pints", "pint" ]), new DictEntry(Data_Units_USCustomary.cup, [ "cups", "cup" ]), new DictEntry(Data_Units_USCustomary.tablespoon, [ "tablespoons", "tablespoon", "tbsp" ]), new DictEntry(Data_Units_USCustomary.teaspoon, [ "teaspoons", "teaspoon", "tsp" ]), new DictEntry(Data_Units_USCustomary.fluidounce, [ "fluidounces", "fluidounce", "floz" ]), new DictEntry(Data_Units_Imperial.furlong, [ "furlong" ]), new DictEntry(Data_Units_Misc.btu, [ "BTU" ]), new DictEntry(Data_Units_Misc.psi, [ "psi" ]), new DictEntry(Data_Units_Misc.mmHg, [ "mmHg" ]), new DictEntry(Data_Units_USCustomary.hogshead, [ "hogsheads", "hogshead" ]), new DictEntry(Data_Units_USCustomary.rod, [ "rods", "rod" ]), new DictEntry(Data_Units.divideUnits(Data_Units_Misc.pixel)(Data_Units_Imperial.inch), [ "ppi" ]), new DictEntry(Data_Units.divideUnits(Data_Units_Misc.dot)(Data_Units_Imperial.inch), [ "dpi" ]), new DictEntry(Data_Units_Misc.piece, [ "pieces", "piece" ]), new DictEntry(Data_Units_Misc.person, [ "persons", "person", "people" ]), new DictEntry(Data_Units_Currency.dollar, [ "dollars", "dollar", "USD", "$" ]), new DictEntry(Data_Units_Currency.euro, [ "euros", "euro", "EUR", "\u20ac" ]), new DictEntry(Data_Units_Nautical.knot, [ "knots", "knot", "kn", "kt" ]), new DictEntry(Data_Units_Nautical.nauticalMile, [ "M", "NM", "nmi" ]) ]);
  var identStart = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Token.letter(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("_"));
  var identLetter = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Token.letter(Data_Identity.monadIdentity))(Text_Parsing_Parser_Token.digit(Data_Identity.monadIdentity)))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("_")))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("'"));
  var foldr1 = function (f) {
      return function (v) {
          var v1 = Data_List.last(v.value1);
          var v2 = Data_List.init(v.value1);
          if (v2 instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Just) {
              return f(v.value0)(Data_Foldable.foldr(Data_List_Types.foldableList)(f)(v1.value0)(v2.value0));
          };
          return v.value0;
      };
  };
  var commands = [ "help", "?", "list", "ls", "ll", "reset", "clear", "cls", "quit", "exit", "copy", "cp" ];
  var insectLanguage = {
      commentStart: "",
      commentEnd: "",
      commentLine: "#",
      nestedComments: false,
      identStart: identStart,
      identLetter: identLetter,
      opStart: Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)([ "+", "-", "*", "\xb7", "\u22c5", "\xd7", "/", "\xf7", "%", "^", "!", "\u2192", "\u279e", "=" ]),
      opLetter: Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)([  ]),
      reservedNames: Data_Semigroup.append(Data_Semigroup.semigroupArray)(commands)([ "\xb9", "\xb2", "\xb3", "\u2074", "\u2075", "\u207b\xb9", "\u207b\xb2", "\u207b\xb3", "\u207b\u2074", "\u207b\u2075", "to", "per" ]),
      reservedOpNames: [ "->", "+", "-", "*", "\xb7", "\u22c5", "\xd7", "/", "\xf7", "%", "^", "!", "**", "=", "," ],
      caseSensitive: true
  };
  var token = Text_Parsing_Parser_Token.makeTokenParser(Data_Identity.monadIdentity)(insectLanguage);
  var $$function = function (env) {
      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(token.identifier)(function (name) {
          var $17 = name === "sum" || name === "product";
          if ($17) {
              return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(name);
          };
          var v = Data_Map_Internal.lookup(Data_Ord.ordString)(name)(env.functions);
          if (v instanceof Data_Maybe.Just) {
              return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(name);
          };
          if (v instanceof Data_Maybe.Nothing) {
              return Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Unknown function '" + (name + "'"));
          };
          throw new Error("Failed pattern match at Insect.Parser (line 353, column 7 - line 355, column 61): " + [ v.constructor.name ]);
      });
  };
  var parens = token.parens;
  var reserved = token.reserved;
  var reservedOp = token.reservedOp;
  var variable = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Insect_Language.Variable.create)(token.identifier);
  var whiteSpace = token.whiteSpace;
  var number = (function () {
      var fromCharArray = (function () {
          var $43 = Data_Functor.map(Data_Functor.functorArray)(Data_String_CodePoints.codePointFromChar);
          return function ($44) {
              return Data_String_CodePoints.fromCodePointArray($43($44));
          };
      })();
      var digits = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Data_Array.some(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(Text_Parsing_Parser_Combinators.withErrorMessage(Data_Identity.monadIdentity)(Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)([ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]))("a digit")))(function (ds) {
          return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(fromCharArray(Data_Array.fromFoldable(Data_Foldable.foldableArray)(ds)));
      });
      var signAndDigits = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.option(Data_Identity.monadIdentity)("+")(Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)([ "+", "-" ])))(function (sign) {
          return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(digits)(function (intPart) {
              return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Data_String_CodePoints.singleton(Data_String_CodePoints.codePointFromChar(sign)) + intPart);
          });
      });
      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(digits)(function (intPart) {
          return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(Control_Apply.apply(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_Semigroup.append(Data_Semigroup.semigroupString))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)(".")))(digits)))(function (mFracPart) {
              var fracPart = Data_Maybe.fromMaybe("")(mFracPart);
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("e"))(function () {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.notFollowedBy(Data_Identity.monadIdentity)(identStart))(function () {
                      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(signAndDigits)(function (sad) {
                          return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))("e" + sad);
                      });
                  });
              }))))(function (mExpPart) {
                  var expPart = Data_Maybe.fromMaybe("")(mExpPart);
                  return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(whiteSpace)(function () {
                      var floatStr = intPart + (fracPart + expPart);
                      var v = Data_Decimal.fromString(floatStr);
                      if (v instanceof Data_Maybe.Just) {
                          var $24 = Data_Decimal["isFinite"](v.value0);
                          if ($24) {
                              return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(v.value0);
                          };
                          return Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("This number is too large");
                      };
                      if (v instanceof Data_Maybe.Nothing) {
                          return Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Parsing of number failed for input '" + (floatStr + "'"));
                      };
                      throw new Error("Failed pattern match at Insect.Parser (line 114, column 3 - line 119, column 79): " + [ v.constructor.name ]);
                  });
              });
          });
      });
  })();
  var command = Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reserved("help"))(reserved("?")))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Insect_Language.Help.value)))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reserved("list"))(reserved("ls")))(reserved("ll")))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Insect_Language.List.value))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reserved("reset"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Insect_Language.Reset.value))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reserved("clear"))(reserved("cls")))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Insect_Language.Clear.value))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reserved("copy"))(reserved("cp")))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Insect_Language.Copy.value))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reserved("quit"))(reserved("exit")))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Insect_Language.Quit.value))))(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity));
  var buildDictParser = function (v) {
      var abbrevParser = function (x) {
          return function (abbrev) {
              return Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)(abbrev))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(x));
          };
      };
      var entryParser = function (v1) {
          return Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))(Data_Functor.map(Data_Functor.functorArray)(abbrevParser(v1.value0))(v1.value1));
      };
      return Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))(Data_Functor.map(Data_Functor.functorArray)(entryParser)(v.value0));
  };
  var imperialUnit = Text_Parsing_Parser_Combinators.withErrorMessage(Data_Identity.monadIdentity)(buildDictParser(imperialUnitDict))("imperial unit");
  var normalUnit = Text_Parsing_Parser_Combinators.withErrorMessage(Data_Identity.monadIdentity)(buildDictParser(normalUnitDict))("normal unit");
  var prefix = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(buildDictParser(prefixDict))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Control_Category.identity(Control_Category.categoryFn)));
  var unitWithSIPrefix = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(prefix)(function (p) {
      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(normalUnit)(function (u) {
          return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(p(u));
      });
  });
  var derivedUnit = (function () {
      var augment = function (p) {
          return Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(p)(Text_Parsing_Parser_Combinators.notFollowedBy(Data_Identity.monadIdentity)(identLetter));
      };
      return Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(augment(unitWithSIPrefix)))(augment(imperialUnit)))(augment(normalUnit)))(augment(specialCases)))(whiteSpace);
  })();
  var expression = function (env) {
      var subOp = reservedOp("-");
      var powPos = function (s) {
          return function (q) {
              if (s === 1.0) {
                  return q;
              };
              if (Data_Boolean.otherwise) {
                  return new Insect_Language.BinOp(Insect_Language.Pow.value, q, Insect_Language.Scalar.create(Data_Decimal.fromNumber(s)));
              };
              throw new Error("Failed pattern match at Insect.Parser (line 460, column 5 - line 461, column 65): " + [ s.constructor.name, q.constructor.name ]);
          };
      };
      var powOp = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reservedOp("^"))(reservedOp("**"));
      var powNeg = function (s) {
          return function (q) {
              return new Insect_Language.BinOp(Insect_Language.Pow.value, q, Insect_Language.Negate.create(Insect_Language.Scalar.create(Data_Decimal.fromNumber(s))));
          };
      };
      var perOp = reserved("per");
      var mulOp = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reservedOp("*"))(reservedOp("\xb7")))(reservedOp("\u22c5")))(reservedOp("\xd7"));
      var modOp = reservedOp("%");
      var facOp = reservedOp("!");
      var divOp = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reservedOp("/"))(reservedOp("\xf7"));
      var commaOp = reservedOp(",");
      var arrOp = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(reservedOp("->"))(reservedOp("\u2192")))(reservedOp("\u279e")))(reserved("to"));
      var addOp = reservedOp("+");
      return Control_Lazy.fix(Text_Parsing_Parser.lazyParserT)(function (p) {
          var atomic = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(whiteSpace)(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(parens(p))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Insect_Language.Scalar.create)(number)))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Insect_Language.Unit.create)(derivedUnit))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.apply(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Insect_Language.Apply.create)($$function(env)))(parens(sepBy1(Data_Identity.monadIdentity)(p)(commaOp))))))(variable));
          var suffixFac = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(atomic)(function (a) {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(facOp)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Insect_Language.Factorial.create))))(function (mf) {
                  if (mf instanceof Data_Maybe.Just) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(mf.value0(a));
                  };
                  if (mf instanceof Data_Maybe.Nothing) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(a);
                  };
                  throw new Error("Failed pattern match at Insect.Parser (line 375, column 9 - line 377, column 27): " + [ mf.constructor.name ]);
              });
          });
          var suffixPow = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(suffixFac)(function (x) {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\xb9"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powPos(1.0))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\xb2"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powPos(2.0)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\xb3"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powPos(3.0)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\u2074"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powPos(4.0)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\u2075"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powPos(5.0)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\u207b\xb9"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powNeg(1.0)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\u207b\xb2"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powNeg(2.0)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\u207b\xb3"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powNeg(3.0)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\u207b\u2074"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powNeg(4.0)))))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(reservedOp("\u207b\u2075"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(powNeg(5.0))))))(function (mFn) {
                  if (mFn instanceof Data_Maybe.Just) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(mFn.value0(x));
                  };
                  if (mFn instanceof Data_Maybe.Nothing) {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(x);
                  };
                  throw new Error("Failed pattern match at Insect.Parser (line 393, column 9 - line 395, column 27): " + [ mFn.constructor.name ]);
              });
          });
          var sepByPow = (function () {
              var list = function (e) {
                  return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(suffixPow)(function (a) {
                      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Data_List.many(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(powOp)(function () {
                          return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(subOp)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Insect_Language.Negate.create)))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(addOp)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Control_Category.identity(Control_Category.categoryFn)))))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Control_Category.identity(Control_Category.categoryFn))))(function (func) {
                              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(e)(function (expr) {
                                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(func(expr));
                              });
                          });
                      })))(function (as) {
                          return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(new Data_NonEmpty.NonEmpty(a, as));
                      });
                  });
              };
              return Control_Lazy.fix(Text_Parsing_Parser.lazyParserT)(function (e) {
                  return Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(foldr1(Insect_Language.BinOp.create(Insect_Language.Pow.value)))(list(e));
              });
          })();
          var sepByMulImplicit = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_NonEmpty.foldl1(Data_List_Types.foldableList)(Insect_Language.BinOp.create(Insect_Language.Mul.value)))(sepBy1(Data_Identity.monadIdentity)(sepByPow)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Data_Unit.unit)));
          var prefixed = Control_Lazy.fix(Text_Parsing_Parser.lazyParserT)(function (e) {
              return Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(subOp)(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Insect_Language.Negate.create)(e)))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(addOp)(e)))(sepByMulImplicit);
          });
          var sepByMod = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_NonEmpty.foldl1(Data_List_Types.foldableList)(Insect_Language.BinOp.create(Insect_Language.Mod.value)))(sepBy1(Data_Identity.monadIdentity)(prefixed)(modOp));
          var sepByPer = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_NonEmpty.foldl1(Data_List_Types.foldableList)(Insect_Language.BinOp.create(Insect_Language.Div.value)))(sepBy1(Data_Identity.monadIdentity)(sepByMod)(perOp));
          var sepByDiv = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_NonEmpty.foldl1(Data_List_Types.foldableList)(Insect_Language.BinOp.create(Insect_Language.Div.value)))(sepBy1(Data_Identity.monadIdentity)(sepByPer)(divOp));
          var sepByMul = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_NonEmpty.foldl1(Data_List_Types.foldableList)(Insect_Language.BinOp.create(Insect_Language.Mul.value)))(sepBy1(Data_Identity.monadIdentity)(sepByDiv)(mulOp));
          var sepBySub = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_NonEmpty.foldl1(Data_List_Types.foldableList)(Insect_Language.BinOp.create(Insect_Language.Sub.value)))(sepBy1(Data_Identity.monadIdentity)(sepByMul)(subOp));
          var sepByAdd = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_NonEmpty.foldl1(Data_List_Types.foldableList)(Insect_Language.BinOp.create(Insect_Language.Add.value)))(sepBy1(Data_Identity.monadIdentity)(sepBySub)(addOp));
          var sepByConv = Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_NonEmpty.foldl1(Data_List_Types.foldableList)(Insect_Language.BinOp.create(Insect_Language.ConvertTo.value)))(sepBy1(Data_Identity.monadIdentity)(sepByAdd)(arrOp));
          return sepByConv;
      });
  };
  var fullExpression = function (env) {
      return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(whiteSpace)(function () {
          return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(expression(env))(function (expr) {
              return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.withErrorMessage(Data_Identity.monadIdentity)(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity))("end of input"))(function () {
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(expr);
              });
          });
      });
  };
  var assignment = function (env) {
      var failIfUnit = function (n) {
          return Control_Applicative.when(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Data_Either.isRight(Text_Parsing_Parser.runParser(n)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(derivedUnit)(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)))))(Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("'" + (n + "' is reserved for a physical unit")));
      };
      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(whiteSpace)(function () {
          return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(token.identifier)(function (name) {
              return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(parens(sepBy1(Data_Identity.monadIdentity)(token.identifier)(reservedOp(",")))))(function (args) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(reservedOp("="))(function () {
                      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(expression(env))(function (expr) {
                          return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity))(function () {
                              return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))({
                                  name: name,
                                  args: args,
                                  expr: expr
                              });
                          });
                      });
                  });
              });
          });
      })))(function (v) {
          return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(failIfUnit(v.name))(function () {
              if (v.args instanceof Data_Maybe.Nothing) {
                  return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(new Insect_Language.VariableAssignment(v.name, v.expr));
              };
              if (v.args instanceof Data_Maybe.Just) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Data_Foldable.traverse_(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Data_NonEmpty.foldableNonEmpty(Data_List_Types.foldableList))(failIfUnit)(v.args.value0))(function () {
                      return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(new Insect_Language.FunctionAssignment(v.name, v.args.value0, v.expr));
                  });
              };
              throw new Error("Failed pattern match at Insect.Parser (line 502, column 3 - line 506, column 45): " + [ v.args.constructor.name ]);
          });
      });
  };
  var statement = function (env) {
      return Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Insect_Language.Command.create)(command))(assignment(env)))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(whiteSpace)(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Insect_Language.PrettyPrintFunction.create)($$function(env))))(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)))))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Insect_Language.Expression.create)(fullExpression(env)));
  };
  var parseInsect = function (env) {
      return function (inp) {
          return Text_Parsing_Parser.runParser(inp)(statement(env));
      };
  };
  exports["commands"] = commands;
  exports["normalUnitDict"] = normalUnitDict;
  exports["imperialUnitDict"] = imperialUnitDict;
  exports["parseInsect"] = parseInsect;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Insect"] = $PS["Insect"] || {};
  var exports = $PS["Insect"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Array = $PS["Data.Array"];
  var Data_Either = $PS["Data.Either"];
  var Data_Map = $PS["Data.Map"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Set = $PS["Data.Set"];
  var Data_Show = $PS["Data.Show"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];
  var Insect_Environment = $PS["Insect.Environment"];
  var Insect_Format = $PS["Insect.Format"];
  var Insect_Interpreter = $PS["Insect.Interpreter"];
  var Insect_Parser = $PS["Insect.Parser"];
  var Insect_PrettyPrint = $PS["Insect.PrettyPrint"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];                
  var supportedUnits = (function () {
      var toStrs = function (v) {
          return v.value1;
      };
      var toArray = function (v) {
          return Control_Bind.bind(Control_Bind.bindArray)(v.value0)(toStrs);
      };
      return Data_Array.sort(Data_Ord.ordString)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(toArray(Insect_Parser.normalUnitDict))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(toArray(Insect_Parser.imperialUnitDict))([ "d", "t" ])));
  })();
  var msgTypeToString = function (v) {
      if (v instanceof Insect_Interpreter.Info) {
          return "info";
      };
      if (v instanceof Insect_Interpreter["Error"]) {
          return "error";
      };
      if (v instanceof Insect_Interpreter.Value) {
          return "value";
      };
      if (v instanceof Insect_Interpreter.ValueSet) {
          return "value-set";
      };
      throw new Error("Failed pattern match at Insect (line 44, column 1 - line 44, column 39): " + [ v.constructor.name ]);
  };
  var initialEnvironment = Insect_Environment.initialEnvironment;
  var identifiers = function (env) {
      return Data_Set.toUnfoldable(Data_Unfoldable.unfoldableArray)(Data_Map.keys(env.values));
  };
  var functions = function (env) {
      return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Set.toUnfoldable(Data_Unfoldable.unfoldableArray)(Data_Map.keys(env.functions)))([ "sum", "product" ]);
  };
  var fmtPlain = Insect_Format.fmtPlain;
  var repl = function (fmt) {
      return function (env) {
          return function (userInput) {
              var v = Insect_Parser.parseInsect(env)(userInput);
              if (v instanceof Data_Either.Left) {
                  var pos = Text_Parsing_Parser.parseErrorPosition(v.value0);
                  return {
                      msg: Insect_Format.format(fmt)([ Insect_Format.optional(Insect_Format.text("  ")), Insect_Format.error("Parse error at position " + (Data_Show.show(Data_Show.showInt)(pos.column) + ": ")), Insect_Format.text(Text_Parsing_Parser.parseErrorMessage(v.value0)) ]),
                      msgType: "error",
                      newEnv: env
                  };
              };
              if (v instanceof Data_Either.Right) {
                  var ans = Insect_Interpreter.runInsect(env)(v.value0);
                  if (ans.msg instanceof Insect_Interpreter.Message) {
                      return {
                          msgType: msgTypeToString(ans.msg.value0),
                          msg: Insect_Format.format(fmt)(ans.msg.value1),
                          newEnv: ans.newEnv
                      };
                  };
                  if (ans.msg instanceof Insect_Interpreter.MQuit) {
                      return {
                          msgType: "quit",
                          msg: "",
                          newEnv: ans.newEnv
                      };
                  };
                  if (ans.msg instanceof Insect_Interpreter.MCopy) {
                      var storedQty = function (v1) {
                          return v1.value1;
                      };
                      var maybeStoredValue = Data_Map_Internal.lookup(Data_Ord.ordString)("ans")(ans.newEnv.values);
                      var value = Data_Maybe.maybe("")(function (sv) {
                          return Insect_Format.format(fmtPlain)(Insect_PrettyPrint.prettyQuantity(storedQty(sv)));
                      })(maybeStoredValue);
                      return {
                          msgType: "copy",
                          msg: value,
                          newEnv: ans.newEnv
                      };
                  };
                  if (ans.msg instanceof Insect_Interpreter.MClear) {
                      return {
                          msgType: "clear",
                          msg: "",
                          newEnv: ans.newEnv
                      };
                  };
                  throw new Error("Failed pattern match at Insect (line 70, column 10 - line 90, column 36): " + [ ans.msg.constructor.name ]);
              };
              throw new Error("Failed pattern match at Insect (line 55, column 3 - line 90, column 36): " + [ v.constructor.name ]);
          };
      };
  };
  var fmtJqueryTerminal = Insect_Format.fmtJqueryTerminal;
  var fmtConsole = Insect_Format.fmtConsole;
  var commands = Insect_Parser.commands;
  exports["repl"] = repl;
  exports["initialEnvironment"] = initialEnvironment;
  exports["supportedUnits"] = supportedUnits;
  exports["fmtPlain"] = fmtPlain;
  exports["fmtJqueryTerminal"] = fmtJqueryTerminal;
  exports["fmtConsole"] = fmtConsole;
  exports["commands"] = commands;
  exports["functions"] = functions;
  exports["identifiers"] = identifiers;
})(PS);module.exports = PS["Insect"];

},{"decimal.js":1}]},{},[2])(2)
});
