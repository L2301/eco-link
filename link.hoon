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
    =/  current-info  (~(gut by roll-call.state) from.tx *info)
    =/  new-info=info  [frozen=%.y]
    =/  new-roll-call  (~(put by roll-call.state) from.tx new-info)
    =/  new-nonces  (~(put by nonces.state) from.tx +(nonce.tx))
    =/  new-state  state(roll-call new-roll-call)
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
--

::
=|  state=link-state
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
    =/  executed  (execute-transaction state tx.task)
    :_  link-gate(state new-state.executed)
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
