::  %link: blockchain world state vane
::  Empty implementation that crashes on everything
::
!:
|=  our=ship
=,  link
=>
|%

+|  %helpers
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
    =/  new-state  state(roll-call new-roll-call, nonces new-nonces)
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
      %spawn  [%.y 0 state]  :: TODO: implement
      %dissolve  [%.y 0 state]  :: TODO: implement
      %create-layer  [%.y 0 state]  :: TODO: implement
      %melt  [%.y 0 state]  :: TODO: implement
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
==
::
+|  %consensus
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
    :_  link-gate(link-state.state new-state.executed)
    :~  [duct %give [%.y !>([%transaction-executed result=executed])]]
    ==
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
