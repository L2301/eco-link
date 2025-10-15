::  %link: blockchain world state vane
::  Empty implementation that crashes on everything
::
!:
|=  our=ship
=,  link
=>
|%

+|  %helpers
::
::
++  freeze
    |=  [state=link-state tx=link-transaction]
    ^-  execution-result
    ?>  ?=(%freeze -.tx)
    ::  Validate nonce
    =/  expected-nonce  (~(gut by nonces.state) from.tx 0)
    ?.  =(expected-nonce nonce.tx)
    [%.n 0 state]  :: Failed, invalid nonce
    ::  Check if account is frozen
    ?:  (is-frozen state from.tx)
      [%.n 1 state]  :: Failed, account is frozen
    ::  Freeze account
    =/  from-balance  (~(gut by balances.state) from.tx 0)
    ::  Check has funds to freeze
    ?:  =(from-balance 0)
        [%.n 1 state]
    ::  Deduct balance (funds held in frozen-data, not credited to layer yet)
    =/  new-balances  (~(put by balances.state) from.tx 0)
    ::  Create frozen-data
    =/  freeze-data=frozen-data
        :*  amount=from-balance
            layer=target-layer.tx
            freeze-block=current-height.state
            last-update=current-height.state
        ==
    =/  current-info  (~(gut by roll-call.state) from.tx *info)
    =/  new-info=info
      current-info(frozen %.y, frozen-data `freeze-data)
    =/  new-roll-call  (~(put by roll-call.state) from.tx new-info)
    =/  new-nonces  (~(put by nonces.state) from.tx +(nonce.tx))
    =/  unfreeze-at  (add current-height.state 100)
    =/  new-pending  (~(put by pending-unfreezes.state) from.tx unfreeze-at)
    =/  new-state  state(balances new-balances, roll-call new-roll-call, nonces new-nonces, pending-unfreezes new-pending)
    [%.y 1 new-state]  :: Success, return new state
::
++  thaw
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%thaw -.tx)
  ::  Validate nonce
  =/  expected-nonce  (~(gut by nonces.state) from.tx 0)
  ?.  =(expected-nonce nonce.tx)
  [%.n 0 state]  :: Failed, invalid nonce
  ::  Check if account is frozen
  ?.  (is-frozen state from.tx)
    [%.n 1 state]  :: Failed, account is not frozen
  ::  Thaw account
  =/  current-info  (~(got by roll-call.state) from.tx)
  =/  new-info  current-info(frozen %.n)
  =/  new-roll-call  (~(put by roll-call.state) from.tx new-info)
  =/  new-nonces  (~(put by nonces.state) from.tx +(nonce.tx))
  =/  new-state  state(roll-call new-roll-call, nonces new-nonces)
  [%.y 1 new-state]  :: Success, return new state
::
++  is-frozen
    |=  [state=link-state who=@p]
    ^-  ?
    =/  account-info  (~(get by roll-call.state) who)
    ?~  account-info  
      %.n
    frozen.u.account-info
::
++  generate-contract-address
  |=  [state=link-state from=@p nonce=@ud]
  ^-  @p
  =/  base=@  (mix from nonce)
  =/  attempt=@ud  0
  |-  
  =/  current=@p  `@p`(mix base attempt)
  ?:  (~(has by contracts.state) current)
    $(attempt +(attempt))
  current
::
++  get-signature
  |=  tx=link-transaction
  ^-  (unit @ux)
  ?-  -.tx
    %transfer  `signature.tx
    %deploy    `signature.tx
    %call      `signature.tx
    %freeze    `signature.tx
    %thaw      `signature.tx
    %melt      ~
    %spawn     ~
    %dissolve  ~
    %create-layer  ~
  ==
::
++  get-from-address
  |=  tx=link-transaction
  ^-  (unit @p)
  ?-  -.tx
    %transfer  `from.tx
    %deploy    `from.tx
    %call      `from.tx
    %freeze    `from.tx
    %thaw      `from.tx
    %melt      ~
    %spawn     ~
    %dissolve  ~
    %create-layer  ~
  ==
::
++  get-nonce
  |=  tx=link-transaction
  ^-  (unit @ud)
  ?-  -.tx
    %transfer  `nonce.tx
    %deploy    `nonce.tx
    %call      `nonce.tx
    %freeze    `nonce.tx
    %thaw      `nonce.tx
    %melt      ~
    %spawn     ~
    %dissolve  ~
    %create-layer  ~
  ==
::
++  strip-signature
  |=  tx=link-transaction
  ^-  *
  ?-  -.tx
    %transfer  [%transfer from.tx to.tx amount.tx nonce.tx]
    %deploy    [%deploy from.tx code.tx initial-state.tx nonce.tx]
    %call      [%call from.tx contract.tx method.tx args.tx nonce.tx]
    %freeze    [%freeze from.tx nonce.tx]
    %thaw      [%thaw from.tx nonce.tx]
    %melt      [%melt from.tx zkp.tx nonce.tx block-height.tx type.tx]
    %spawn     tx
    %dissolve  tx
    %create-layer  tx
  ==
::
++  validate-signature
  |=  [state=link-state tx=link-transaction]
  ^-  ?
  =/  from  (get-from-address tx)
  ?~  from  %.n  :: No from address means system tx, shouldn't call this
  =/  sig  (get-signature tx)
  ?~  sig  %.n  :: No signature
  =/  account-info  (~(got by roll-call.state) u.from)
  =/  msg=@  (sham (strip-signature tx))
  ::  verify using ed25519
  (veri:ed:crypto u.sig msg pass.account-info)
::
++  execute-transfer
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%transfer -.tx)
  ::  Check if account is frozen
  ?:  (is-frozen state from.tx)
    [%.n 0 state]  :: Account frozen, no gas charged
  ::  Validate nonce
  =/  expected-nonce  (~(gut by nonces.state) from.tx 0)
  ?.  =(expected-nonce nonce.tx)
    [%.n 0 state]  :: Failed, invalid nonce
  =/  from-balance  (~(gut by balances.state) from.tx 0)
  ::  Check sufficient balance
  ?:  (lth from-balance amount.tx)
    [%.n 0 state]  :: Failed, return unchanged state
  ::  Execute transfer
  =/  new-balances
    =.  balances.state  (~(put by balances.state) from.tx (sub from-balance amount.tx))
    =/  to-balance  (~(gut by balances.state) to.tx 0)
    (~(put by balances.state) to.tx (add to-balance amount.tx))
  =/  new-nonces  (~(put by nonces.state) from.tx +(nonce.tx))
  =/  new-state  state(balances new-balances, nonces new-nonces)
  [%.y 1 new-state]  :: Success, return new state
::
++  deploy-contract
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%deploy -.tx)
  ::  Check if account is frozen
  ?:  (is-frozen state from.tx)
    [%.n 0 state]  :: Account frozen, no gas charged
  ::  Validate nonce
  =/  expected-nonce  (~(gut by nonces.state) from.tx 0)
  ?.  =(expected-nonce nonce.tx)
    [%.n 0 state]  :: Failed, invalid nonce
  ::  Generate contract address
  =/  contract-address  (generate-contract-address state from.tx nonce.tx)
  ::  Check if contract already exists
  ?:  (~(has by contracts.state) contract-address)
    [%.n 0 state]  :: Failed, contract already exists
  ::  Create new contract
  =/  new-contract
    :*  code=code.tx
        state=initial-state.tx
        owner=from.tx
    ==
  =/  new-contracts  (~(put by contracts.state) contract-address new-contract)
  =/  new-nonces  (~(put by nonces.state) from.tx +(nonce.tx))
  =/  new-state  state(contracts new-contracts, nonces new-nonces)
  [%.y 2 new-state]  :: Success, return new state
::
++  call-contract
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%call -.tx)
  ::  Check if account is frozen
  ?:  (is-frozen state from.tx)
    [%.n 0 state]  :: Account frozen, no gas charged
  ::  Validate nonce
  =/  expected-nonce  (~(gut by nonces.state) from.tx 0)
  ?.  =(expected-nonce nonce.tx)
    [%.n 0 state]
  ::  Check contract exists
  ?~  maybe-contract=(~(get by contracts.state) contract.tx)
    [%.n 1 state]  :: Contract not found, 1 gas
  ::  Get contract
  =/  contract  u.maybe-contract
  =/  contract-subject  [method.tx args.tx from.tx state.contract]
  ::  Slam the method arm with args
  =/  slam-result  (mule |.(.*(code.contract [9 2 10 [6 contract-subject] 0 1])))
  ::  Check if execution crashed
  ?:  ?=(%| -.slam-result)
    [%.n 3 state]  :: Contract execution failed, 3 gas
  ::  Extract [new-state result]
  ?:  ?=([* *] p.slam-result)
    [%.n 4 state]  :: Contract execution failed, 4 gas
  =/  result-cell  ;;([* *] p.slam-result)
  =/  [new-contract-state=* result=*]  result-cell
  ::  Update contract state
  =/  updated-contract  contract(state new-contract-state)
  =/  new-contracts  (~(put by contracts.state) contract.tx updated-contract)
  =/  new-nonces  (~(put by nonces.state) from.tx +(nonce.tx))
  =/  new-state  state(contracts new-contracts, nonces new-nonces)
  [%.y 5 new-state]  :: Success, 5 gas
::
++  execute-transaction
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?:  ?=(?(%spawn %dissolve %create-layer %melt) -.tx)
    ?-  -.tx
      %spawn  (execute-spawn state tx)
      %dissolve  (execute-dissolve state tx)
      %create-layer  (execute-create-layer state tx)
      %melt  (execute-melt state tx)  :: TODO: implement
    ==
  ::  Validate signature
  ?.  (validate-signature state tx)
    [%.n 0 state]  :: Failed, invalid signature
  ::  check nonce
  =/  current-nonce  (~(gut by nonces.state) from.tx 0)
  =/  nonce  (get-nonce tx)
  ?.  =(nonce current-nonce)
    [%.n 0 state]  :: Failed, invalid nonce
  ?-  -.tx
  %transfer
    (execute-transfer state tx)
  %deploy
    (deploy-contract state tx)
  %call
    (call-contract state tx)
  %freeze
    (freeze state tx)
  %thaw
    (thaw state tx)
  ==
::
++  execute-melt
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%melt -.tx)
  ::  TODO: Validate ZKP (placeholder for now)
  ::  ?.  (validate-layer-zkp zkp.tx from.tx block-height.tx)
  ::  [%.n 0 state]
  ::  Process based on melt type
  ?-  -.type.tx
    %withdrawal
  =/  who=@p  who.type.tx
  ::
  ::  Check account is frozen
  =/  maybe-info  (~(get by roll-call.state) who)
  ?~  maybe-info  [%.n 0 state]  ::  Account doesn't exist
  ?.  frozen.u.maybe-info  [%.n 0 state]  ::  Not frozen
  ?~  frozen-data.u.maybe-info  [%.n 0 state]  ::  No frozen data
  ::
  ::  Check frozen for THIS layer
  ?.  =(layer.u.frozen-data.u.maybe-info from.tx)
    [%.n 0 state]  ::  Frozen for different layer
  ::
  ::  Get withdrawal amount from ZKP
  ::  (In reality, extract from zkp.tx; for now, use frozen amount)
  =/  withdrawal-amount=@ud  amount.u.frozen-data.u.maybe-info
  ::
  ::  Check layer has sufficient balance (custody check)
  =/  layer-balance=@ud  (~(gut by balances.state) from.tx 0)
  ?.  (gte layer-balance withdrawal-amount)
    [%.n 0 state]  ::  Insufficient custody - layer is insolvent!
  ::
  ::  Execute withdrawal
  ::  Debit layer
  =.  balances.state
    (~(put by balances.state) from.tx (sub layer-balance withdrawal-amount))
  ::  Credit user
  =.  balances.state
    (~(put by balances.state) who withdrawal-amount)
  ::
  ::  Unfreeze account
  =/  new-info  u.maybe-info(frozen %.n, frozen-data ~)
  =.  roll-call.state  (~(put by roll-call.state) who new-info)
  ::
  ::  Remove from pending unfreezes
  =.  pending-unfreezes.state  (~(del by pending-unfreezes.state) who)
  ::
  [%.y 0 state]
  ::
  ::  STATE-UPDATE: Accept frozen accounts (existing implementation)
  ::
    
    %state-update
    ::  Accept frozen accounts by crediting layer and updating last-update
    =/  new-state=link-state  state
    =/  accounts=(list @p)  accounts.type.tx
    |-
    ?~  accounts
      [%.y 0 new-state]
    =/  who=@p  i.accounts
    ::  Check if account is frozen waiting for this layer
    =/  maybe-info  (~(get by roll-call.new-state) who)
    ?~  maybe-info
      $(accounts t.accounts)  :: Account doesn't exist, skip
    ?.  frozen.u.maybe-info
      $(accounts t.accounts)  :: Not frozen, skip
    ?~  frozen-data.u.maybe-info
      $(accounts t.accounts)  :: No frozen data, skip
    ?.  =(layer.u.frozen-data.u.maybe-info from.tx)
      $(accounts t.accounts)  :: Frozen for different layer, skip
    ::  Credit layer balance (accepting the frozen funds)
    =/  layer-balance  (~(gut by balances.new-state) from.tx 0)
    =/  new-balances  (~(put by balances.new-state) from.tx (add layer-balance amount.u.frozen-data.u.maybe-info))
    ::  Update frozen-data.last-update
    =/  updated-frozen-data  u.frozen-data.u.maybe-info(last-update current-height.new-state)
    =/  updated-info  u.maybe-info(frozen-data `updated-frozen-data)
    =/  new-roll-call  (~(put by roll-call.new-state) who updated-info)
    =/  new-pending  (~(del by pending-unfreezes.new-state) who)
    $(accounts t.accounts, new-state new-state(balances new-balances, roll-call new-roll-call, pending-unfreezes new-pending))
    
    %full-state
    ::  Similar to state-update but for all accounts in zkp
    [%.y 0 state]  :: TODO
  ==
::
++  execute-spawn
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%spawn -.tx)
  ::
  ::  System transaction - no signature validation needed
  ::  Check account doesn't already exist
  ?:  (~(has by roll-call.state) who.tx)
    [%.n 0 state]  ::  Account already exists
  ::
  ::  Extract state from frozen snapshot
  =/  balance  (~(gut by balances.frozen-state.tx) who.tx 0)
  =/  nonce    (~(gut by nonces.frozen-state.tx) who.tx 0)
  =/  life     (~(gut by lives.frozen-state.tx) who.tx 1)
  =/  pass     (~(gut by passes.frozen-state.tx) who.tx 0x0)
  ::
  ::  Create account in roll-call
  =/  new-info=info
    :*  frozen=%.n
        life=life
        pass=pass
        frozen-data=~
    ==
  =/  new-roll-call  (~(put by roll-call.state) who.tx new-info)
  ::
  ::  Set balance and nonce
  =/  new-balances  (~(put by balances.state) who.tx balance)
  =/  new-nonces    (~(put by nonces.state) who.tx nonce)
  ::
  [%.y 0 state(roll-call new-roll-call, balances new-balances, nonces new-nonces)]
::
++  execute-create-layer
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%create-layer -.tx)
  ::
  ::  TODO: Validate controller owns the address
  ::  TODO: Record layer creation in some registry
  ::  For now, just succeed
  [%.y 0 state]
::
++  execute-dissolve
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%dissolve -.tx)
  ::
  ::  Get layer identity from location
  =/  layer-id=@p
    ?-  -.layer.tx
      %l1  ~|(%cannot-dissolve-l1 !!)
      %l2  galaxy.layer.tx
      %l3  star.layer.tx
    ==
  ::
  ::  Get layer's custody pool
  =/  pool=@ud  (~(gut by balances.state) layer-id 0)
  ::
  ::  Find all frozen accounts for this layer
  =/  all-accounts=(list [@p info])  ~(tap by roll-call.state)
  =/  frozen-for-layer=(list [@p frozen-data])
    %+  murn  all-accounts
    |=  [who=@p acc=info]
    ?.  frozen.acc  ~
    ?~  frozen-data.acc  ~
    ?.  =(layer.u.frozen-data.acc layer-id)  ~
    `[who u.frozen-data.acc]
  ::
  ::  Calculate total owed (from L1's stale perspective)
  =/  total-owed=@ud
    %+  roll  frozen-for-layer
    |=  [[who=@p fdata=frozen-data] acc=@ud]
    (add acc amount.fdata)
  ::
  ::  Pro-rata distribute
  =|  new-balances=_balances.state
  =.  new-balances  balances.state
  =|  new-roll-call=_roll-call.state
  =.  new-roll-call  roll-call.state
  ::
  =/  accounts-to-process  frozen-for-layer
  |-  ^-  execution-result
  ?~  accounts-to-process
    ::  All accounts processed, clear layer's balance
    =.  new-balances  (~(put by new-balances) layer-id 0)
    [%.y 0 state(balances new-balances, roll-call new-roll-call)]
  ::
  =/  [who=@p fdata=frozen-data]  i.accounts-to-process
  ::
  ::  Calculate recovery (pro-rata based on L1's knowledge)
  =/  recovery=@ud
    ?:  =(total-owed 0)  0
    (div (mul amount.fdata pool) total-owed)
  ::
  ::  Credit account
  =/  current-balance  (~(gut by new-balances) who 0)
  =.  new-balances  (~(put by new-balances) who (add current-balance recovery))
  ::
  ::  Unfreeze account
  =/  account-info  (~(got by new-roll-call) who)
  =/  updated-info  account-info(frozen %.n, frozen-data ~)
  =.  new-roll-call  (~(put by new-roll-call) who updated-info)
  ::
  $(accounts-to-process t.accounts-to-process)
::
+|  %block
::
+$  block
  $:  hash=@uvH
      parent-hash=@uvH
      height=@ud
      timestamp=@da
      transactions=(list link-transaction)
      consensus-data=consensus-proof
      state-root=@uvH
      creator=@p
  ==
::
+$  consensus-proof
  $%  [%nakamoto nonce=@ud difficulty=@ud]
  ==
::
+$  chain-state
  $:  blocks=(map @uvH block)
      block-height=(map @ud @uvH)  :: height -> block hash
      best-tip=@uvH
      difficulty=@ud
==
::
+|  %consensus
::
++  execute-mempool-transactions
  |=  [ls=link-state txs=(list link-transaction)]
  ^-  [(list link-transaction) link-state]
  =/  executed=(list link-transaction)  ~
  =/  current-state=link-state  ls
  |-
  ?~  txs
    [(flop executed) current-state]  :: Reverse to maintain order
  =/  result=execution-result  (execute-transaction current-state i.txs)
  ?:  success.result
    %=  $
      txs  t.txs
      executed  [i.txs executed]
      current-state  new-state.result
    ==
  ::  Skip failed transaction
  $(txs t.txs)
::
++  produce-block
  |=  [ls=link-state cs=chain-state mempool=(list link-transaction) now=@da our=@p]
  ^-  [block link-state chain-state]
  ::  Get parent block
  =/  parent-hash=@uvH  best-tip.cs
  =/  parent=block  (~(got by blocks.cs) parent-hash)
  ::  Execute mempool transactions
  =/  [executed-txs=(list link-transaction) new-ls=link-state]
    (execute-mempool-transactions ls mempool)
  ::  Compute state root
  =/  state-root=@uvH  (compute-state-root new-ls)
  ::  Build block template
  =/  blk=block
    :*  hash=*@uvH  :: Will be set by mining
        parent-hash=parent-hash
        height=+(height.parent)
        timestamp=now  :: TODO: need now passed in
        transactions=executed-txs
        consensus-data=[%nakamoto nonce=0 difficulty=difficulty.cs]
        state-root=state-root
        creator=our  :: TODO: need our passed in
    ==
  ::  Mine the block
  =/  mined=block  (mine-block blk difficulty.cs cs)
  ::
  =.  new-ls  (process-pending-unfreezes new-ls)
  ::  Increment height in link-state
  =.  new-ls  new-ls(current-height +(height.mined))
  ::  Update chain state
  =/  new-blocks  (~(put by blocks.cs) hash.mined mined)
  =/  new-height  (~(put by block-height.cs) height.mined hash.mined)
  =/  updated-cs  cs(blocks new-blocks, block-height new-height, best-tip hash.mined)
  [mined new-ls updated-cs]
::
++  mine-block
  |=  [blk=block target-difficulty=@ud cs=chain-state]
  ^-  block
  =/  nonce=@ud  0
  |-
  =/  candidate=block
    blk(consensus-data [%nakamoto nonce target-difficulty])
  =/  candidate-hash=@uvH  (compute-block-hash candidate)
  =/  candidate-with-hash=block  candidate(hash candidate-hash)
  =/  parent=block  (~(got by blocks.cs) parent-hash.blk)
  ?:  (validate-consensus candidate-with-hash parent target-difficulty)
    candidate-with-hash
  $(nonce +(nonce))
::
++  validate-consensus
  |=  [blk=block parent=block target-difficulty=@ud]
  ^-  ?
  ?>  ?=(%nakamoto -.consensus-data.blk)
  ::  Check timestamp is after parent
  ?.  (gth timestamp.blk timestamp.parent)
    %.n
  ::  Check height increments correctly
  ?.  =(height.blk +(height.parent))
    %.n
  ::  Check hash meets difficulty (leading zeros)
  ::  Lower hash value = more leading zeros = harder to find
  =/  hash-num=@  `@`hash.blk
  =/  target=@  (sub (bex 256) (bex (sub 256 target-difficulty)))
  (lte hash-num target)
::
++  compute-block-hash
  |=  blk=block
  ^-  @uvH
  ::  Hash everything except the hash field itself
  =/  hashable
    :*  parent-hash.blk
        height.blk
        timestamp.blk
        transactions.blk
        consensus-data.blk
        state-root.blk
        creator.blk
    ==
  `@uvH`(sham hashable)
::
++  compute-state-root
  |=  ls=link-state
  ^-  @uvH
  ::  Hash the entire world state
  `@uvH`(sham [balances.ls contracts.ls nonces.ls roll-call.ls])
::
++  difficulty-target
  |=  difficulty=@ud
  ^-  @
  ::  Convert difficulty (number of leading zero bits) to target number
  ::  Higher difficulty = smaller target = harder to find valid hash
  (sub (bex 256) (bex (sub 256 difficulty)))
::
++  select-best-tip
  |=  [cs=chain-state new-hash=@uvH]
  ^-  @uvH
  =/  current-tip=block  (~(got by blocks.cs) best-tip.cs)
  =/  new-block=block  (~(got by blocks.cs) new-hash)
  ::  Longest chain wins
  ?:  (gth height.new-block height.current-tip)
    new-hash
  best-tip.cs
::
++  validate-block
  |=  [ls=link-state cs=chain-state blk=block]
  ^-  [? link-state chain-state]
  ::  Check parent exists
  ?~  maybe-parent=(~(get by blocks.cs) parent-hash.blk)
    [%.n ls cs]
  =/  parent=block  u.maybe-parent
  ::  Validate consensus
  ?.  (validate-consensus blk parent difficulty.cs)
    [%.n ls cs]
  ::  Validate hash
  =/  computed-hash=@uvH  (compute-block-hash blk)
  ?.  =(hash.blk computed-hash)
    [%.n ls cs]
  ::  Re-execute all transactions to verify state root
  =/  tx-state=link-state  ls
  =/  txs=(list link-transaction)  transactions.blk
  |-
  ?~  txs
    ::  All transactions executed, check state root
    =/  final-state-root=@uvH  (compute-state-root tx-state)
    ?.  =(state-root.blk final-state-root)
      [%.n ls cs]
    ::  Block valid - update chain
    ::  Increment height
    =.  tx-state  tx-state(current-height height.blk)
    =/  new-blocks  (~(put by blocks.cs) hash.blk blk)
    =/  new-height  (~(put by block-height.cs) height.blk hash.blk)
    =/  new-tip  (select-best-tip cs hash.blk)
    =/  updated-cs  cs(blocks new-blocks, block-height new-height, best-tip new-tip)
    [%.y tx-state updated-cs]
  ::  Execute next transaction
  =/  result  (execute-transaction tx-state i.txs)
  ?.  success.result
    [%.n ls cs]  ::  Transaction failed, block invalid
  $(txs t.txs, tx-state new-state.result)
::
++  process-pending-unfreezes
  |=  state=link-state
  ^-  link-state
  ::
  =/  pending=(list [@p @ud])  ~(tap by pending-unfreezes.state)
  =|  new-state=link-state
  =.  new-state  state
  ::
  |-  ^-  link-state
  ?~  pending  new-state
  ::
  =/  [who=@p expiry-height=@ud]  i.pending
  ::  Check if expired
  ?.  (lte expiry-height current-height.new-state)
    $(pending t.pending)  ::  Not expired yet
  ::
  ::  Expired - process refund
  =/  maybe-info  (~(get by roll-call.new-state) who)
  ?~  maybe-info
    ::  Account gone somehow, just remove from pending
    $(pending t.pending, new-state new-state(pending-unfreezes (~(del by pending-unfreezes.new-state) who)))
  ::
  ?.  frozen.u.maybe-info
    ::  Not frozen anymore (maybe already processed?), remove from pending
    $(pending t.pending, new-state new-state(pending-unfreezes (~(del by pending-unfreezes.new-state) who)))
  ::
  ?~  frozen-data.u.maybe-info
    ::  No frozen data, shouldn't happen but skip
    $(pending t.pending, new-state new-state(pending-unfreezes (~(del by pending-unfreezes.new-state) who)))
  ::
  ::  Actually refund
  =/  refund=@ud  amount.u.frozen-data.u.maybe-info
  =/  current-balance  (~(gut by balances.new-state) who 0)
  =/  new-balances  (~(put by balances.new-state) who (add current-balance refund))
  ::
  ::  Unfreeze account
  =/  new-info  u.maybe-info(frozen %.n, frozen-data ~)
  =/  new-roll-call  (~(put by roll-call.new-state) who new-info)
  ::
  ::  Remove from pending
  =/  new-pending  (~(del by pending-unfreezes.new-state) who)
  ::
  %=  $
    pending  t.pending
    new-state  new-state(balances new-balances, roll-call new-roll-call, pending-unfreezes new-pending)
  ==
::
--

::
=|  state=[link-state=link-state chain-state=chain-state mempool=(list link-transaction)]
|=  [now=@da eny=@uvJ rof=*]                :: simplified roof type
|%
::  Vane interface
::
++  call
  |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _link-gate]
  =/  =task  ((harden task) wrapped-task)
  ?-  -.task
    %born
    [~ link-gate]
  ::
    %trim
    [~ link-gate]
  ::
    %vega
    [~ link-gate]
  ::
    %execute-transaction
    =/  executed  (execute-transaction link-state.state tx.task)
    ?:  success.executed
      :_  link-gate(link-state.state new-state.executed)
      :~  [duct %give [%.y !>([%transaction-executed result=executed])]]
      ==
    :_  link-gate
    :~  [duct %give [%.n !>([%transaction-failed result=executed])]]
    ==
  ::
    %validate-block
    =/  blk=block  ;;(block block.task)
    =/  [valid=? new-ls=link-state new-cs=chain-state]
      (validate-block link-state.state chain-state.state blk)
    ?:  valid
      :_  link-gate(link-state.state new-ls, chain-state.state new-cs)
      :~  [duct %give [%.y !>([%block-validated blk])]]
      ==
    :_  link-gate
    :~  [duct %give [%.n !>([%block-invalidated blk])]]
    ==
  ::
    %add-to-mempool
    =/  new-mempool  [tx.task mempool.state]
    ::  Check if should produce block
    =/  mempool-size=@ud  (lent new-mempool)
    =/  should-produce-tx  (gte mempool-size 100)  :: Hardcoded threshold
    =/  current-tip=block  (~(got by blocks.chain-state.state) best-tip.chain-state.state)
    =/  time-since-block  (sub now timestamp.current-tip)
    =/  should-produce-time  (gth time-since-block ~s60)  :: 1 minute
    ?:  |(should-produce-tx should-produce-time)
      ::  Produce block
      =/  [blk=block new-ls=link-state new-cs=chain-state]
        (produce-block link-state.state chain-state.state new-mempool now our)
      :_  link-gate(link-state.state new-ls, chain-state.state new-cs, mempool.state ~)
      :~  [duct %give [%.y !>([%new-block blk])]]
      ==
    ::  Just add to mempool, no block production
    [~ link-gate(mempool.state new-mempool)]
  ==
::
++  take
  |=  [=wire =duct dud=(unit goof) sign=*]
  ^-  [(list move) _link-gate]
  [~ link-gate]
::
++  link-gate  ..$
::
++  load
  |=  old=*
  ^+  ..$
  ..$
::
++  stay  link-state
::
++  scry
    ^-  roon 
    |=  [lyc=gang pov=path car=term bem=beam]
    ^-  (unit (unit cage))
    ~
--
