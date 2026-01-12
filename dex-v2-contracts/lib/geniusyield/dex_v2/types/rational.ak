// SPDX-License-Identifier: BSL-1.1
// Copyright              : Copyright (c) 2025 GYELD GmbH
// License                : Business Source License 1.1
// Licensed Work          : GeniusYield Onchain Scripts
// Change License         : GPL v2.0 or later (after Change Date)
// See LICENSE file for full terms

//// Defines type for rational number.

/// Rational number. Note that there are no checks performed to ensure that denominator is non-zero & positive. Order creation logic is expected to ensure this. Such invariants is the reason that validator should only consider those orders which have the required NFT.
pub type Rational {
  numerator: Int,
  denominator: Int,
}

pub fn verify_rational(r: Rational) -> Bool {
  r.denominator > 0
}

test ceil_1() {
  and {
    ceil(Rational(1, 2)) == 1,
    ceil(Rational(16, 3)) == 6,
    ceil(Rational(-13, 3)) == -4,
  }
}

pub fn ceil(r: Rational) -> Int {
  ( r.numerator + r.denominator - 1 ) / r.denominator
}

pub fn floor(r: Rational) -> Int {
  r.numerator / r.denominator
}

pub const zero = Rational { numerator: 0, denominator: 1 }

pub const one = Rational { numerator: 1, denominator: 1 }

/// Create rational number from an integer.
pub fn from_int(n: Int) -> Rational {
  Rational { numerator: n, denominator: 1 }
}

/// Multiply two rational numbers.
pub fn mul(left: Rational, right: Rational) -> Rational {
  Rational {
    numerator: left.numerator * right.numerator,
    denominator: left.denominator * right.denominator,
  }
}

/// Add two rational numbers.
pub fn add(left: Rational, right: Rational) -> Rational {
  Rational {
    numerator: left.numerator * right.denominator + right.numerator * left.denominator,
    denominator: left.denominator * right.denominator,
  }
}

/// Invert rational number.
pub fn inv(r: Rational) -> Option<Rational> {
  if r.numerator == 0 {
    None
  } else {
    if r.numerator < 0 {
      Some(Rational { numerator: -r.denominator, denominator: -r.numerator })
    } else {
      Some(Rational { numerator: r.denominator, denominator: r.numerator })
    }
  }
}
