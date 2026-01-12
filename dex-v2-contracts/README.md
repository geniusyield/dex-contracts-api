# Two-Way Order Smart Contracts

## Intent and design goals

The Two-Way Order (TWO) contracts manage limit orders that can either be one-way (single sided liquidity) or two-way (both sides funded). The design targets four goals:

1. **Deterministic minting** – Each order NFT name is derived from the consumed transaction input, making order IDs collision-free without extra state.
2. **Permissioned administration with transparent rules** – A configuration datum stored as an inline reference input centralizes protocol parameters (fees, oracle window, stake credentials) and is signed by a multisig.
3. **Fair fills and cancels** – Stake validators enforce the accounting of maker/taker fees, oracle freshness, and validity intervals so that takers cannot underpay and makers cannot bypass withdrawal permissions.
4. **Composable transactions** – Mint, fill, and cancel paths all accept batched inputs/outputs while proving minimal facts (e.g. NFT name order) so off-chain builders can remain efficient.

## Architecture overview

The suite is split into four scripts so we can keep minting lightweight and share on-chain configuration:

- `order_validator_mint` is the minting policy for order NFTs. It reads the configuration via a marked reference input and verifies every freshly minted output (see [`validators/order.ak`](validators/order.ak)).
- `order_validator_spend` is the spending script on the order UTxO. It only checks that the correct stake credential withdrawal authorizes the action (fill vs cancel) and otherwise defers to the stake validators.
- `order_validator_stake_fill` consumes the fill stake credential. It drives fills by folding over inputs/outputs with [`traverse_fill_inputs_outputs`](lib/geniusyield/dex_v2/order.ak).
- `order_validator_stake_cancel` consumes the cancel stake credential. It similarly walks inputs/outputs with [`traverse_cancel_inputs_outputs`](lib/geniusyield/dex_v2/order.ak).

All validators expect the configuration datum to be referenced through the unique `ref_nft` asset passed in as a parameter so that upgrades are gated by the multisig (the datum structure is documented below).

## Datum and redeemer schema

### Mint redeemer (Plutus data)

```text
Constr 0 [
  Constr 0 [
    Constr 0 [
      B "0000000000000000000000000000000000000000000000000000000000000000",
      I 0
    ],
    I 1
  ]
]
```

### `ConfigurationDatum`

Source: [`lib/geniusyield/dex_v2/types/order.ak`](lib/geniusyield/dex_v2/types/order.ak).

| Field                       | Type                        | Purpose                                                                                        |
| --------------------------- | --------------------------- | ---------------------------------------------------------------------------------------------- |
| `total_signatories`         | `List<VerificationKeyHash>` | Full list of keys allowed to rotate configuration parameters.                                  |
| `required_signatories`      | `Int`                       | Threshold for configuration updates.                                                           |
| `nft_policy_id`             | `PolicyId`                  | Policy ID for order NFTs used by both mint and stake validators.                               |
| `fill_hash` / `cancel_hash` | `ScriptHash`                | Hashes of the fill/cancel stake validators used to gate withdrawals.                           |
| `fee_addr`                  | `Address`                   | Protocol-fee sink; fee outputs must pay here during fills.                                     |
| `maker_lovelace_flat_fee`   | `Int`                       | Flat ADA charge that must be deposited with each new order.                                    |
| `maker_fee_ratio`           | `Rational`                  | Maker percentage fee charged on the consumed side during fills.                                |
| `taker_lovelace_flat_fee`   | `Int`                       | Maximum lovelace takers pay per transaction (the stake validator takes the max across inputs). |
| `taker_fee_ratio`           | `Rational`                  | Taker percentage fee applied to payment and offer assets.                                      |
| `oracle_freshness_seconds`  | `Int`                       | Maximum age for oracle quotes. Applies to delta-oracle orders.                                 |
| `min_deposit`               | `Int`                       | Minimum ADA deposit that must sit in each order UTxO.                                          |

> **Operational note:** Lowering `min_deposit` after orders have been placed will cause the validator to accept only the new minimum in continuing outputs. Transaction builders must explicitly refund any excess lovelace to the maker; otherwise, third-party fillers can legally pocket the difference.

```text
Constr 0 [
  List [
    B "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
  ],
  I 1,
  B "00000000000000000000000000000000000000000000000000000001",
  B "138afbec1455a17d72263b6f8fc3092c56cc32dce1ab1ae161c7e00d",
  B "0700cd4d1688ef0899d8083aa9e9890669bef905beefdcab493c84e3",
  Constr 0 [
    Constr 0 [
      B "903884936fec86ccd3e6133e289e7f950b709a60bf64af0f9dc64410"
    ],
    Constr 1 []
  ],
  I 3000000,
  Constr 0 [
    I 3,
    I 1000
  ],
  I 900000,
  Constr 0 [
    I 3,
    I 1000
  ],
  I 60,
  I 3000000
]
```

### `OrderDatum`

```text
Constr 0 [
  Constr 0 [
    List [
      B "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
    ],
    List []
  ],
  Constr 0 [
    Constr 0 [
      B "903884936fec86ccd3e6133e289e7f950b709a60bf64af0f9dc64410"
    ],
    Constr 1 []
  ],
  B "017f9c9e31ac8256ca2f258583df262dbc7d6f68f2a03043d5c99a4ae5",
  Constr 0 [
    Constr 0 [
      B "dda5fdb1002f7389b33e036b6afee82a8189becb6cba852e8b79b4fb",
      B "0014df1047454e53"
    ],
    I 1000000
  ],
  Constr 0 [
    Constr 0 [
      B "",
      B ""
    ],
    I 0
  ],
  Constr 0 [
    Constr 0 [
      I 2,
      I 1
    ],
    Constr 1 []
  ],
  Constr 1 [],
  Constr 1 [],
  I 900000,
  Constr 0 [
    I 3,
    I 1000
  ],
  Constr 0 [
    I 3,
    I 1000
  ],
  I 60
]
```

Source: [`lib/geniusyield/dex_v2/types/order.ak`](lib/geniusyield/dex_v2/types/order.ak).

An order tracks both legs (`a`/`b`), validity windows, fee configuration, and oracle memoization. Important invariants enforced by [`verify_output`](lib/geniusyield/dex_v2/order.ak) include:

- NFT name, payment address, owner multisig, fee ratios, and oracle freshness seconds must match the configuration and remain unchanged across fills.【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L86-L138】【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L680-L701】
- Asset classes `a` and `b` must differ and their amounts respect fill direction (one side decreases, the counter-side receives payment).【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L122-L137】【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L700-L725】
- Validators enforce fees entirely through the dedicated fee output (see fee accounting below).

| Field                      | Type             | Purpose                                                                                                                                      |
| -------------------------- | ---------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| `owner`                    | `MultisigScript` | Authorises cancellation; contains owner verification keys/scripts.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L182-L189】         |
| `payment_addr`             | `Address`        | Destination for ADA/assets returned when the order closes.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L190-L194】                 |
| `nft_name`                 | `AssetName`      | Order NFT identifier that must match the minted token name.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L195-L200】                |
| `a`                        | `AssetDetails`   | Offered asset class and available amount (must stay non-negative).【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L201-L207】         |
| `b`                        | `AssetDetails`   | Counter asset class and amount (zero for one-way orders).【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L201-L207】                  |
| `price`                    | `Price`          | Fixed or oracle-based pricing for fills (optionally includes reverse leg).【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L208-L222】 |
| `validity_start`           | `Option<Int>`    | Lower validity bound in POSIX milliseconds (optional).【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L223-L231】                     |
| `validity_end`             | `Option<Int>`    | Upper validity bound in POSIX milliseconds (optional).【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L223-L231】                     |
| `taker_lovelace_flat_fee`  | `Int`            | Flat ADA charged to takers per fill transaction.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L241-L246】                           |
| `taker_fee_ratio`          | `Rational`       | Percentage fee applied to taker payments.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L247-L252】                                  |
| `maker_fee_ratio`          | `Rational`       | Percentage debited from maker allocation per fill.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L253-L258】                         |
| `oracle_freshness_seconds` | `Int`            | Maximum acceptable oracle quote age when filling.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L259-L263】                          |

Supporting datums:

- `AssetDetails` ensures non-negative balances for each leg.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L19-L36】
- `Price` supports fixed and oracle-delta prices, optionally with reverse quotes for two-way orders.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L66-L110】
- `OneWayDatum` is the inline datum on the burn output when a one-way order is fully consumed; it carries the NFT name and optional oracle proof to close the order.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L60-L64】

### Redeemers

#### `MintRedeemer`

Source: [`lib/geniusyield/dex_v2/types/order.ak`](lib/geniusyield/dex_v2/types/order.ak).

| Field          | Type                         | Purpose                                                                                                                                                                        |
| -------------- | ---------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `MintRedeemer` | `Option<OutputReferenceInt>` | `Some` indicates minting; carries the consumed UTxO reference plus minted count. `None` represents pure burns.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L12-L31】 |
| `output_ref`   | `OutputReference`            | Input used to derive the NFT name (transaction id + index).【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L14-L19】                                                    |
| `amount`       | `Int`                        | Number of NFTs minted/burned in this redeemer.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L20-L23】                                                                 |

#### `OrderRedeemer`

Source: [`lib/geniusyield/dex_v2/types/order.ak`](lib/geniusyield/dex_v2/types/order.ak).

| Constructor   | Purpose                                                                                                                                    |
| ------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `CancelOrder` | Proves ownership via the cancel stake credential and burns the NFT.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L38-L42】        |
| `FillOrder`   | Authorises fills via the fill stake credential and updates liquidity/fees.【F:aiken/dex-v2/lib/geniusyield/dex_v2/types/order.ak†L42-L45】 |

## Validation logic

### Minting policy (`order_validator_mint`)

1. Compute the positive mints for the policy and reject burns-only redeemers that mint extra tokens.【F:aiken/dex-v2/validators/order.ak†L12-L30】
2. When minting, fetch `ConfigurationDatum` via the reference NFT, require that the specified `output_ref` is consumed, and derive the exact NFT names through [`populate_nft_set`](lib/geniusyield/dex_v2/order.ak).【F:aiken/dex-v2/validators/order.ak†L31-L64】【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L32-L73】
3. Enforce `0 < amount < 256` (the counter byte budget) and exact equality between the derived NFT multiset and the minted tokens.【F:aiken/dex-v2/validators/order.ak†L47-L64】
4. Fold over all transaction outputs via [`verify_outputs`](lib/geniusyield/dex_v2/order.ak) so each NFT-bearing output is inline-datum verified, value-checked against `min_deposit`, and its datum fields validated (fee ratios, oracle freshness, owner, etc.).【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L75-L173】

### Spending script (`order_validator_spend`)

The on-chain spending script is intentionally small: it ensures the withdrawal set includes the right stake credential for the requested action, preventing fills and cancels from happening in the same transaction and delegating heavy checks to the stake validators.【F:aiken/dex-v2/validators/order.ak†L66-L95】

### Fill stake validator (`order_validator_stake_fill`)

1. Load the configuration datum from the reference input and ensure the cancel stake credential is not also withdrawing in the same transaction.【F:aiken/dex-v2/validators/order.ak†L99-L132】
2. Call [`traverse_fill_inputs_outputs`](lib/geniusyield/dex_v2/order.ak) to fold through every input carrying an order NFT. The helper:
   - Skips unrelated inputs and recurses on the rest.【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L290-L345】
   - For each order, confirms validity windows and finds the matching output via [`traverse_fill_outputs`](lib/geniusyield/dex_v2/order.ak).【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L329-L365】
   - Handles one-way burns (order fully consumed) by verifying oracle signatures, computing payment/fees, and asserting the owner payout (ADA min deposit + payment leg).【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L500-L575】
   - Handles continuing outputs by recalculating amounts, requiring unchanged metadata, enforcing oracle freshness/reverse pricing, and updating fee accumulators for taker and maker percentages.【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L576-L737】
3. After all inputs are processed, require that burns match minted negatives, compute positive taker fee aggregates, and demand a dedicated fee output paying the maker and taker accumulators plus the flat lovelace charge when required.【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L300-L330】

### Cancel stake validator (`order_validator_stake_cancel`)

1. Load the configuration datum and ensure no fill withdrawals are present alongside cancels.【F:aiken/dex-v2/validators/order.ak†L134-L170】
2. [`traverse_cancel_inputs_outputs`](lib/geniusyield/dex_v2/order.ak) walks each order input, gathers the owner multisig signatures/scripts, and ensures all NFTs are burned.【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L740-L821】
3. [`traverse_cancel_outputs`](lib/geniusyield/dex_v2/order.ak) checks that each order’s assets plus min deposit return to the `payment_addr` and that the NFT is handed back (inline datum `InlineDatum(nft_asset_name)` so the mint policy can see it later).【F:aiken/dex-v2/lib/geniusyield/dex_v2/order.ak†L821-L900】

## Fee accounting

The stake validator derives fees from the *relationships* between the pre-fill order UTxO and the continuing output. For each order input:

1. **Identify the consumed leg.** Compare `a.amount` and `b.amount` between the input datum and the continuing datum. The asset that decreases defines `A_gross = before - after`. If the fill burns the order (no continuing output) the gross amount is the original `a.amount` (for straight fills) or `b.amount` (for reverse fills).
2. **Compute the net delivered amount.** Let the maker ratio be `r = maker_fee_ratio = p/q`. The validator takes

   ```
   A_net = ceil(A_gross * q / (p + q))
   maker_fee = A_gross - A_net
   ```

   This exactly solves the fixed point `A_gross = A_net + floor(A_net * r)` when it exists and otherwise chooses the smallest `A_net` that makes the inequality hold. NFT fills (`A_gross = 1`) therefore keep `A_net = 1` with zero maker fee.
3. **Price the payment leg on the net.** The amount the taker must contribute is `B_net = ceil(A_net * price)` for straight fills and `ceil(A_net * reverse_price)` for reverse fills. Continuing outputs *only* grow by `B_net` on the opposite asset; takers never pay for the maker portion.
4. **Accumulate taker percentage fees.** The validator tracks two running values:
   - `floor(B_net * taker_fee_ratio)` is added to the payment asset.
   - `floor(A_net * taker_fee_ratio)` is subtracted from the offer asset (a “virtual” negative fee).  
     When multiple fills share a transaction these negatives can cancel positives from the other direction; only the positive remainder is enforced on the fee output.
5. **Flat taker fee.** If any maker/taker percentage fee is positive or any order actually consumes assets, the maximum `taker_lovelace_flat_fee` observed across all filled orders must be paid in lovelace.

When ADA is one of the order legs, remember that the datum amounts exclude the `min_deposit`. Always subtract the deposit from the raw lovelace held in the UTxO before computing `A_gross` or `B_net`; the validator adds the deposit back automatically when building the required continuation value.

### Worked examples

The table below uses the default ratios `maker_fee_ratio = taker_fee_ratio = 0.003` (3 bps). All amounts are integers; prices are rationals in units of “payment over offer”.

1. **Straight fill, non-ADA pair (large order)**  
   `A_before = 100 000 000 000`, `A_after = 89 970 000 000` → `A_gross = 10 030 000 000`.  
   `A_net = ceil(10 030 000 000 / 1.003) = 10 000 000 000`, maker fee `= 30 000 000`.  
   Price `8/5` ⇒ `B_net = ceil(10 000 000 000 × 8/5) = 16 000 000 000`.  
   Taker percentage delta `= floor(16 000 000 000 × 0.003) − floor(10 000 000 000 × 0.003) = 18 000 000` (paid in the price asset).

2. **Straight fill, non-ADA pair (small fragment)**  
   `A_gross = 25 075`, `A_net = 25 000`, maker fee `= 75`.  
   Price `3/2` ⇒ `B_net = 37 500`, taker delta `= 37`.  
   This fragment-sized example matches the first leg in a multi-fill transaction.

3. **Reverse fill, non-ADA pair (large order)**  
   `B_before = 30 000 000 000`, `B_after = 17 462 387 162` → `B_gross = 12 537 612 838`.  
   `B_net = ceil(12 537 612 838 / 1.003) = 12 500 000 000`, maker fee `= 37 612 838`.  
   Reverse price `2/3` ⇒ `A_net = ceil(12 500 000 000 × 2/3) = 8 333 333 334`.  
   Taker delta `= floor(8 333 333 334 × 0.003) − floor(12 500 000 000 × 0.003) = −12 500 000` (clamped to zero; no extra payment fee is required).

4. **Reverse fill, non-ADA pair (small fragment)**  
   `B_gross = 50 150`, `B_net = 50 000`, maker fee `= 150`.  
   Reverse price `3/5` ⇒ `A_net = 30 000`, taker delta `= −60`.

5. **Straight fill with ADA as the offer leg**  
   `A_before = 15 000 000 000`, `A_after = 14 498 495 487` → `A_gross = 501 504 513`.  
   `A_net = 500 000 000`, maker fee `= 1 504 513`.  
   Price `5/4` ⇒ `B_net = 625 000 000`, taker delta `= 375 000`.  
   The 3 000 000 lovelace deposit is unaffected; only the datum amount participates in the computation.

6. **Reverse fill with ADA as the payment leg**  
   `B_before = 12 000 000 000`, `B_after = 10 796 389 168` → `B_gross = 1 203 610 832`.  
   `B_net = 1 200 000 000`, maker fee `= 3 610 832`.  
   Straight price `4/3` ⇒ `A_net = 1 600 000 000`, taker delta `= −1 200 000` (no extra ADA is required beyond the flat fee when applicable).

7. **NFT fill**  
   `A_gross = 1` ⇒ `A_net = 1`, maker fee `= 0`. NFTs never pay maker fees.

8. **Two straight fragments in one transaction**  
   - Fragment 1: `A_gross = 25 075`, `A_net = 25 000`, maker fee `= 75`, `B_net = 37 500`.  
   - Fragment 2: `A_gross = 10 531`, `A_net = 10 500`, maker fee `= 31`, `B_net = 15 750`.  
   Totals: `A_gross = 35 606`, `A_net = 35 500`, maker fee `= 106`, `B_net = 53 250`. Percent deltas add across fragments before the validator enforces the single fee output.

9. **Mixed straight + reverse fragments (taker fee cancellation)**  
   - Straight fragment: `A_gross = 70 210`, `A_net = 70 000`, maker fee `= 210`, `B_net = 93 334`.  
   - Reverse fragment: `B_gross = 50 150`, `B_net = 50 000`, maker fee `= 150`, `A_net = 30 000`.  
   Taker delta across the pair `= 10`, showing that equal-and-opposite directions almost cancel the percentage fee, leaving only a minimal residual.

10. **Rounding edge case**  
   `A_gross = 334` ⇒ `A_net = 334`, maker fee `= 0`, yet `floor(A_net × 0.003) = 1`. The validator tolerates these situations because it prices from the derived net, so takers still pay one unit in the price asset even though the maker fee rounds down to zero.

### Mint-time flat fee

Every order mint transaction must still pay `order_count × maker_lovelace_flat_fee` lovelace to `ConfigurationDatum.fee_addr` in a dedicated output. The output carries an inline datum equal to the NFT policy ID so the minting policy can confirm it belongs to the active blueprint. Builders should *not* preload this flat fee inside individual order UTxOs; aggregate it in the fee output alongside the configuration reference input. The helper `mk_fee_output` in [`tests/order.ak`](tests/order.ak) models the required shape for single and batched placements, and validator tests reject transactions that omit, mis-tag, or underfund this fee output.【F:aiken/dex-v2/validators/order.ak†L45-L92】【F:aiken/dex-v2/validators/tests/order.ak†L205-L390】

## Test coverage

`aiken check` exercises the mint, fill, cancel, and oracle freshness paths under [`validators/tests/order.ak`](validators/tests/order.ak):

- `assess_mint_costs` and `mint_multiple_orders_mixed_types` assert NFT naming, datum equality, and value equality for new orders.【F:aiken/dex-v2/validators/tests/order.ak†L205-L274】【F:aiken/dex-v2/validators/tests/order.ak†L347-L520】
- `mint_rejects_missing_fee_output`, `mint_rejects_fee_output_wrong_datum`, and `mint_rejects_insufficient_maker_fee_payment` prove the flat maker fee output must exist, carry the policy-id inline datum, and pay the full amount.【F:aiken/dex-v2/validators/tests/order.ak†L285-L390】
- `mint_rejects_negative_freshness` / `mint_accepts_updated_freshness` guard configuration bounds on `oracle_freshness_seconds`.【F:aiken/dex-v2/validators/tests/order.ak†L275-L346】
- `fill_one_way_fixed_partial_charges_maker_fee_per_fill` and `taker_fee_cancel_out_two_leg_route` cover partial fills, fee accumulation, and multi-leg cancellation of taker fees.【F:aiken/dex-v2/validators/tests/order.ak†L521-L655】
- `fill_rejects_missing_fee_output`, `fill_rejects_fee_output_wrong_datum`, and `fill_rejects_insufficient_fee_output_value` ensure the stake validator demands the dedicated fee output with correct datum and value totals for maker/taker fees and the flat charge.【F:aiken/dex-v2/validators/tests/order.ak†L745-L1013】
- `oracle_timestamp_fresh_and_stale_inclusive` / `oracle_timestamp_freshness_only` cover oracle freshness helpers.【F:aiken/dex-v2/validators/tests/order.ak†L656-L699】
- `cancel_two_orders_batch` ensures cancel flows enforce multisig witness collection and burn accounting.【F:aiken/dex-v2/validators/tests/order.ak†L700-L790】

## Examples
### September 24, 2025 Preprod Batch
- Transaction: `267981cc35dd54b7ef3129259ea916e00286dfe0b6de158be3dc84133b8b293e` (Preprod)
- Orders reside at `addr_test1wq478dn9rnuq36ytgnjg0dmtpnw5terh7hkhd694epp4ufcsgtvc2`; all four share the order NFT policy `ca41ae6e5b7e1dff8a8a2e0344da7c9728f1258edd0561403fd5858d` minted during the 2025-09-24 redeploy.
- Data pulled via Maestro with `resolve_datums=true`; amounts reflect on-chain units (lovelace for ADA).

| Order | UTxO | NFT name (hex)   | Offer (`a`) | Offer amt | Consideration (`b`) | Cons amt | Price straight | Reverse |
| ----- | ---- | ---------------- | ----------- | --------: | ------------------- | -------: | -------------- | ------- |
| 1     | #1   | `0145b76b…9e9b6` | GOLD        |         1 | SILVER              |        0 | 2/1            | –       |
| 2     | #2   | `0245b76b…9e9b6` | SILVER      |         2 | GOLD                |        1 | 1/2            | 1/1     |
| 3     | #3   | `0345b76b…9e9b6` | COPPER      |         5 | ADA                 |        0 | 600000/1       | –       |
| 4     | #4   | `0445b76b…9e9b6` | GOLD        |         3 | ADA                 |        0 | 4/3            | –       |

_Prices encode units of `b` per unit of `a`; a dash indicates `reverse = None`._

#### Order 1 — One-way GOLD → SILVER
- **UTxO**: `267981cc35dd54b7ef3129259ea916e00286dfe0b6de158be3dc84133b8b293e#1`
- **NFT unit**: `ca41ae6e5b7e1dff8a8a2e0344da7c9728f1258edd0561403fd5858d0145b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6`
- **Value**: 3,000,000 lovelace + 1 GOLD + 1 order NFT
- **Offer (`a`)**: GOLD (fbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c6593, asset name `474f4c44`) (amount 1)
- **Consideration (`b`)**: SILVER (3f9375992bc3846201d56e897106470717a587c46f66b12a140e1e1c, asset name `53494c564552`) (amount 0)
- **Price**: straight 2/1 (no reverse leg)
- **Fees**: maker 3/1000 (~0.300%), taker 3/1000 (~0.300%), flat taker 1,500,000 lovelace, oracle freshness 300 s

`Datum bytes`
```
d8799fd8799f9f581cd08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1ff80ffd8799fd8799f581cd08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1ffd87a80ff581d0145b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6d8799fd8799f581cfbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c659344474f4c44ff01ffd8799fd8799f581c3f9375992bc3846201d56e897106470717a587c46f66b12a140e1e1c4653494c564552ff00ffd8799fd8799f0201ffd87a80ffd87a80d87a801a0016e360d8799f031903e8ffd8799f031903e8ff19012cff
```

`BuiltinData`
```
Constr 0 [
  Constr 0 [
    List [
      B "d08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1"
    ],
    List []
  ],
  Constr 0 [
    Constr 0 [
      B "d08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1"
    ],
    Constr 1 []
  ],
  B "0145b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6",
  Constr 0 [
    Constr 0 [
      B "fbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c6593",
      B "474f4c44"
    ],
    I 1
  ],
  Constr 0 [
    Constr 0 [
      B "3f9375992bc3846201d56e897106470717a587c46f66b12a140e1e1c",
      B "53494c564552"
    ],
    I 0
  ],
  Constr 0 [
    Constr 0 [
      I 2,
      I 1
    ],
    Constr 1 []
  ],
  Constr 1 [],
  Constr 1 [],
  I 1500000,
  Constr 0 [
    I 3,
    I 1000
  ],
  Constr 0 [
    I 3,
    I 1000
  ],
  I 300
]
```

#### Order 2 — Two-way SILVER → GOLD
- **UTxO**: `267981cc35dd54b7ef3129259ea916e00286dfe0b6de158be3dc84133b8b293e#2`
- **NFT unit**: `ca41ae6e5b7e1dff8a8a2e0344da7c9728f1258edd0561403fd5858d0245b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6`
- **Value**: 3,000,000 lovelace + 2 SILVER + 1 GOLD + 1 order NFT
- **Offer (`a`)**: SILVER (3f9375992bc3846201d56e897106470717a587c46f66b12a140e1e1c, asset name `53494c564552`) (amount 2)
- **Consideration (`b`)**: GOLD (fbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c6593, asset name `474f4c44`) (amount 1)
- **Price**: straight 1/2; reverse 1/1
- **Fees**: maker 3/1000 (~0.300%), taker 3/1000 (~0.300%), flat taker 1,500,000 lovelace, oracle freshness 300 s

`Datum bytes`
```
d8799fd8799f9f581cd08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1ff80ffd8799fd8799f581cd08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1ffd87a80ff581d0245b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6d8799fd8799f581c3f9375992bc3846201d56e897106470717a587c46f66b12a140e1e1c4653494c564552ff02ffd8799fd8799f581cfbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c659344474f4c44ff01ffd8799fd8799f0102ffd8799fd8799f0101ffffffd87a80d87a801a0016e360d8799f031903e8ffd8799f031903e8ff19012cff
```

`BuiltinData`
```
Constr 0 [
  Constr 0 [
    List [
      B "d08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1"
    ],
    List []
  ],
  Constr 0 [
    Constr 0 [
      B "d08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1"
    ],
    Constr 1 []
  ],
  B "0245b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6",
  Constr 0 [
    Constr 0 [
      B "3f9375992bc3846201d56e897106470717a587c46f66b12a140e1e1c",
      B "53494c564552"
    ],
    I 2
  ],
  Constr 0 [
    Constr 0 [
      B "fbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c6593",
      B "474f4c44"
    ],
    I 1
  ],
  Constr 0 [
    Constr 0 [
      I 1,
      I 2
    ],
    Constr 0 [
      Constr 0 [
        I 1,
        I 1
      ]
    ]
  ],
  Constr 1 [],
  Constr 1 [],
  I 1500000,
  Constr 0 [
    I 3,
    I 1000
  ],
  Constr 0 [
    I 3,
    I 1000
  ],
  I 300
]
```

#### Order 3 — One-way COPPER → ADA
- **UTxO**: `267981cc35dd54b7ef3129259ea916e00286dfe0b6de158be3dc84133b8b293e#3`
- **NFT unit**: `ca41ae6e5b7e1dff8a8a2e0344da7c9728f1258edd0561403fd5858d0345b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6`
- **Value**: 3,000,000 lovelace + 5 COPPER + 1 order NFT
- **Offer (`a`)**: COPPER (2917ed1e3a166be5b152da3c33509fcb337cec96f2be40124ac8d03a, asset name `434f50504552`) (amount 5)
- **Consideration (`b`)**: ADA (amount 0)
- **Price**: straight 600000/1 (no reverse leg)
- **Fees**: maker 3/1000 (~0.300%), taker 3/1000 (~0.300%), flat taker 1,500,000 lovelace, oracle freshness 300 s

`Datum bytes`
```
d8799fd8799f9f581cd08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1ff80ffd8799fd8799f581cd08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1ffd87a80ff581d0345b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6d8799fd8799f581c2917ed1e3a166be5b152da3c33509fcb337cec96f2be40124ac8d03a46434f50504552ff05ffd8799fd8799f4040ff00ffd8799fd8799f1a000927c001ffd87a80ffd87a80d87a801a0016e360d8799f031903e8ffd8799f031903e8ff19012cff
```

`BuiltinData`
```
Constr 0 [
  Constr 0 [
    List [
      B "d08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1"
    ],
    List []
  ],
  Constr 0 [
    Constr 0 [
      B "d08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1"
    ],
    Constr 1 []
  ],
  B "0345b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6",
  Constr 0 [
    Constr 0 [
      B "2917ed1e3a166be5b152da3c33509fcb337cec96f2be40124ac8d03a",
      B "434f50504552"
    ],
    I 5
  ],
  Constr 0 [
    Constr 0 [
      B "",
      B ""
    ],
    I 0
  ],
  Constr 0 [
    Constr 0 [
      I 600000,
      I 1
    ],
    Constr 1 []
  ],
  Constr 1 [],
  Constr 1 [],
  I 1500000,
  Constr 0 [
    I 3,
    I 1000
  ],
  Constr 0 [
    I 3,
    I 1000
  ],
  I 300
]
```

#### Order 4 — One-way GOLD → ADA
- **UTxO**: `267981cc35dd54b7ef3129259ea916e00286dfe0b6de158be3dc84133b8b293e#4`
- **NFT unit**: `ca41ae6e5b7e1dff8a8a2e0344da7c9728f1258edd0561403fd5858d0445b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6`
- **Value**: 3,000,000 lovelace + 3 GOLD + 1 order NFT
- **Offer (`a`)**: GOLD (fbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c6593, asset name `474f4c44`) (amount 3)
- **Consideration (`b`)**: ADA (amount 0)
- **Price**: straight 4/3 (no reverse leg)
- **Fees**: maker 3/1000 (~0.300%), taker 3/1000 (~0.300%), flat taker 1,500,000 lovelace, oracle freshness 300 s

`Datum bytes`
```
d8799fd8799f9f581cd08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1ff80ffd8799fd8799f581cd08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1ffd87a80ff581d0445b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6d8799fd8799f581cfbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c659344474f4c44ff03ffd8799fd8799f4040ff00ffd8799fd8799f0403ffd87a80ffd87a80d87a801a0016e360d8799f031903e8ffd8799f031903e8ff19012cff
```

`BuiltinData`
```
Constr 0 [
  Constr 0 [
    List [
      B "d08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1"
    ],
    List []
  ],
  Constr 0 [
    Constr 0 [
      B "d08a5f8309b6dcd1d912b17309bcf45335d11c07eae1b2a8509c4fa1"
    ],
    Constr 1 []
  ],
  B "0445b76b50904780ce5ea4e5732a3c8d44493f9cf06c49376cdba9e9b6",
  Constr 0 [
    Constr 0 [
      B "fbfbadd44437e7ead14b52868d84f3d042e85cce1cf98919f44c6593",
      B "474f4c44"
    ],
    I 3
  ],
  Constr 0 [
    Constr 0 [
      B "",
      B ""
    ],
    I 0
  ],
  Constr 0 [
    Constr 0 [
      I 4,
      I 3
    ],
    Constr 1 []
  ],
  Constr 1 [],
  Constr 1 [],
  I 1500000,
  Constr 0 [
    I 3,
    I 1000
  ],
  Constr 0 [
    I 3,
    I 1000
  ],
  I 300
]
```
