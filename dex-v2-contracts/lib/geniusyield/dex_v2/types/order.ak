//// Types related to order validator.

use aiken/crypto.{ScriptHash, Signature, VerificationKey, VerificationKeyHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference}
use geniusyield/dex_v2/types/assets.{AssetClass} as gy_assets
use geniusyield/dex_v2/types/multisig.{MultisigScript}
use geniusyield/dex_v2/types/rational.{Rational, verify_rational}

/// Creating new tokens.
pub type OutputReferenceInt {
  /// Reference to the output that is being consumed.
  output_ref: OutputReference,
  /// Amount of tokens being created.
  amount: Int,
}

/// If `Option` is `None`, then we are only burning tokens, else we are also creating new tokens (might still be burning some).
pub type MintRedeemer =
  Option<OutputReferenceInt>

// TODO: All the fields of the datum must be classified in regards to whether their semantic check is required or not.

/// Details of an asset involved in an order.
pub type AssetDetails {
  /// Asset class.
  asset_class: AssetClass,
  /// Available amount of this asset.
  amount: Int,
}

/// Validate asset details. Check that amount is non-negative. Note that since we'll check the output value, we don't need to check if length of `policy_id` is correct.
pub fn validate_asset_details(AssetDetails { amount, .. }: AssetDetails) -> Bool {
  amount >= 0
}

// TODO: If we don't allow complete fill, we can actually support both redeemers as part of single transaction.

/// Available order actions. Note that per transaction, only one common action is allowed across all involved orders. I.e., if two orders are being consumed in same transaction, redeemer for both should be same. This is required so that we don't confuse order cancellation with order fill, as it is difficult to distinguish between the two when folding over multiple orders in our stake validator.
pub type OrderRedeemer {
  /// "Owner" of the author triggers "order cancellation". Note that owner can cancel multiple orders in a single transaction. If one wants to update their position, they can cancel the order and create a new one in the same transaction.
  CancelOrder
  /// Fill, potentially multiple, orders.
  FillOrder
}

// FIXME: Check if it's fine that resulting price due to offset & spread can be negative.
/// Price delta to be applied to oracle price input to get for final price. Actual price is `o * (1 + spread) + offset` where `o` is the oracle price.
pub type PriceDelta {
  offset: Rational,
  spread: Rational,
}

pub type OneWayDatum {
  nft_asset_name: AssetName,
  moracle_fill_details: Option<OracleFillDetails>,
}

/// Price that is set in the order. Note that if you would not like order to be closed when completely filled, set `reverse` to some non-`None` value such that it is detrimental to fill it in reverse direction.
pub type Price {
  /// Fixed price.
  Fixed { straight: Rational, reverse: Option<Rational> }
  /// Price given as a delta to oracle price.
  DeltaOracle {
    // TODO: To validate verification key in order creation?
    oracle_key: VerificationKey,
    straight: PriceDelta,
    reverse: Option<PriceDelta>,
  }
}

/// Validate price.
pub fn validate_price(p: Price, a: AssetDetails, b: AssetDetails) -> Bool {
  // Price (when given as rational) should be positive.
  let validate_price_rational =
    fn(r: Rational) -> Bool { verify_rational(r) && r.numerator > 0 }
  // Rational number should be represented correctly, i.e., denominator is positive whereas numerator can be negative for both spread & offset.
  let validate_price_delta =
    fn(d: PriceDelta) -> Bool {
      verify_rational(d.offset) && verify_rational(d.spread)
    }
  // In case reverse price is not set, we require that no reverse side liquidity is provided and offered amount is positive.
  let one_way_amount_check = a.amount > 0 && b.amount == 0
  when p is {
    Fixed { straight, reverse } -> and {
        validate_price_rational(straight),
        when reverse is {
          None -> one_way_amount_check
          Some(r) -> validate_price_rational(r)
        },
      }
    DeltaOracle { straight, reverse, .. } -> and {
        validate_price_delta(straight),
        when reverse is {
          None -> one_way_amount_check
          Some(r) -> validate_price_delta(r)
        },
      }
  }
}

pub type OracleFillDetails {
  price_signature: Signature,
  price_msg: PriceTimestamp,
}

/// Price along with timestamp.
pub type PriceTimestamp {
  /// Base asset (asset A in the price pair).
  base_asset: gy_assets.AssetClass,
  /// Quote asset (asset B in the price pair).
  quote_asset: gy_assets.AssetClass,
  /// The price.
  price: Rational,
  /// Timestamp when this price was signed/measured (POSIX milliseconds).
  timestamp: Int,
}

// TODO: Price/Amount data structure, together with
// lib.geniusyield.dex_v2.order.traverse_fill_outputs
// could be refactored to safer and suit better actual user cases.
// E.g. to avoid reverse side value in case if there's no reverse side price.
//
/// Order datum.
pub type OrderDatum {
  /// Owner of the order who has the right to cancel it.
  owner: MultisigScript,
  /// Address to receive payment when position is closed. When an order closes, every asset held in the output (except the identifying NFT) is returned to this address.
  payment_addr: Address,
  /// Name of the NFT that is used to identify this order.
  nft_name: AssetName,
  /// Details of asset `a`.
  a: AssetDetails,
  /// Details of asset `b`.
  b: AssetDetails,
  /// See [`Price`](#Price).
  price: Price,
  /// Posix timestamp after this order is valid for filling.
  validity_start: Option<Int>,
  /// Posix timestamp after this order is no longer valid for filling.
  validity_end: Option<Int>,
  /// Flat taker fee in lovelace required to be paid when filling this order. Note that if we are filling multiple orders in a single transaction, we take maximum `taker_lovelace_flat_fee` from all orders.
  taker_lovelace_flat_fee: Int,
  /// Fee ratio that taker has to pay when filling this order.
  /// Represented as a rational fraction; e.g. 0.3% = Rational { numerator: 3, denominator: 1000 }.
  taker_fee_ratio: Rational,
  /// Fee ratio that gets deducted from maker's allocated fee when filling this order.
  /// Represented as a rational fraction; e.g. 0.3% = Rational { numerator: 3, denominator: 1000 }.
  maker_fee_ratio: Rational,
  /// Maximum oracle price age (seconds) accepted when filling this order.
  oracle_freshness_seconds: Int,
}

/// Redeemer payload for stake fills. Supplies fresh oracle data keyed by order NFT names.
pub type FillRedeemer {
  oracle_prices: List<(AssetName, OracleFillDetails)>,
}

/// Validate time interval.
pub fn validate_time_interval(start: Option<Int>, end: Option<Int>) -> Bool {
  when (start, end) is {
    (None, None) -> True
    (Some(s), None) -> s > 0
    (None, Some(e)) -> e > 0
    (Some(s), Some(e)) -> s > 0 && e > 0 && s <= e
  }
}

// pub type FeeOutputDatum =
//   ScriptHash

/// Fee output datum. It's a type synonym around `ScriptHash` and represents hash of the order validator. Since we are doing transaction level validation, to distinguish b/w fee output corresponding to different scripts, we insert the script hash in the datum.
/// Order configuration datum.
pub type ConfigurationDatum {
  /// Set of authorized signatories.
  total_signatories: List<VerificationKeyHash>,
  /// Number of required signatories to authorize an update.
  required_signatories: Int,
  /// Order NFT minting policy.
  nft_policy_id: PolicyId,
  /// Order Fill stake validator.
  fill_hash: ScriptHash,
  /// Order cancel stake validator.
  cancel_hash: ScriptHash,
  /// Address to receive fees.
  fee_addr: Address,
  /// Flat fee in lovelace that is required to be paid when creating an order.
  maker_lovelace_flat_fee: Int,
  /// Fee ratio that maker has to pay when creating an order.
  maker_fee_ratio: Rational,
  /// Flat fee in lovelace that is required to be paid when filling an order.
  taker_lovelace_flat_fee: Int,
  /// Fee ratio that taker has to pay when filling an order.
  taker_fee_ratio: Rational,
  /// Maximum oracle price age (seconds) accepted when filling orders.
  oracle_freshness_seconds: Int,
  /// Minimum lovelace deposit required to create an order. This is eventually returned back to author of the order.
  min_deposit: Int,
}
