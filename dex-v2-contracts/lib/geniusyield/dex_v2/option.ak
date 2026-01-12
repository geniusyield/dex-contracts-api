// SPDX-License-Identifier: BSL-1.1
// Copyright              : Copyright (c) 2025 GYELD GmbH
// License                : Business Source License 1.1
// Licensed Work          : GeniusYield Onchain Scripts
// Change License         : GPL v2.0 or later (after Change Date)
// See LICENSE file for full terms

//// Utility functions for working with `Option` types.

/// Assert that an option is `Some`, returning the value inside.
pub fn assert(val: Option<a>) -> a {
  when val is {
    None -> fail @"Expected Some"
    Some(a) -> a
  }
}
