// SPDX-License-Identifier: BSL-1.1
// Copyright              : Copyright (c) 2025 GYELD GmbH
// License                : Business Source License 1.1
// Licensed Work          : GeniusYield Onchain Scripts
// Change License         : GPL v2.0 or later (after Change Date)
// See LICENSE file for full terms

use aiken/primitive/bytearray
use cardano/assets.{AssetName, PolicyId}
use geniusyield/dex_v2/address.{validate_credential_length}

/// Asset class.
pub type AssetClass {
  policy_id: PolicyId,
  asset_name: AssetName,
}

/// Check if two asset classes are equal.
pub fn is_equal(a: AssetClass, b: AssetClass) -> Bool {
  a.policy_id == b.policy_id && a.asset_name == b.asset_name
}

/// Validate asset class. Checks that policy id is of correct length (28 bytes) and asset name is less than or equal to 32 bytes.
pub fn validate_asset_class(ac: AssetClass) -> Bool {
  validate_credential_length(ac.policy_id) && bytearray.length(ac.asset_name) <= 32
}

/// Type synonym to denote concatenation of policy id & asset name.
pub type AssetClassBA =
  ByteArray
