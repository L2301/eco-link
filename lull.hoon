::  lull: %link type definitions
::  Types for the blockchain world state vane
::
|%
::
::  %link types
::
+|  %link-types
::
::  Account info in the roll-call registry
::
+$  info
  $:  frozen=?
      life=@ud
      pass=@ux
      frozen-data=(unit frozen-data)
  ==
::
::  Data about a frozen account
::
+$  frozen-data
  $:  amount=@ud
      layer=@p
      freeze-block=@ud
      last-update=@ud
  ==
::
::  Smart contract data
::
+$  contract
  $:  code=*
      state=*
      owner=@p
  ==
::
::  Layer location identifier
::
+$  layer-location
  $%  [%l1 ~]
      [%l2 galaxy=@p]
      [%l3 star=@p]
  ==
::
::  Frozen state snapshot (for spawn)
::
+$  frozen-state
  $:  balances=(map @p @ud)
      nonces=(map @p @ud)
      lives=(map @p @ud)
      passes=(map @p @ux)
  ==
::
::  Melt operation type
::
+$  melt-type
  $%  [%withdrawal who=@p]
      [%state-update accounts=(list @p)]
      [%full-state ~]
  ==
::
::  Transaction result
::
+$  execution-result
  [success=? gas-used=@ud new-state=link-state]
::
+|  %link-state
::
::  The blockchain world state
::
+$  link-state
  $:  balances=(map @p @ud)
      contracts=(map @p contract)
      nonces=(map @p @ud)
      roll-call=(map @p info)
      current-height=@ud
      pending-unfreezes=(map @p @ud)
  ==
::
+|  %link-transactions
::
::  Transaction tagged union
::
+$  link-transaction
  $%  $:  %transfer
          from=@p
          to=@p
          amount=@ud
          nonce=@ud
          signature=@ux
      ==
      $:  %deploy
          from=@p
          code=*
          initial-state=*
          nonce=@ud
          signature=@ux
      ==
      $:  %call
          from=@p
          contract=@p
          method=@t
          args=*
          nonce=@ud
          signature=@ux
      ==
      $:  %freeze
          from=@p
          nonce=@ud
          target-layer=@p
          signature=@ux
      ==
      $:  %thaw
          from=@p
          nonce=@ud
          signature=@ux
      ==
      $:  %melt
          from=@p
          zkp=*
          nonce=@ud
          block-height=@ud
          type=melt-type
      ==
      $:  %spawn
          who=@p
          frozen-state=frozen-state
      ==
      [%dissolve layer=layer-location]
      $:  %create-layer
          controller=@p
          layer=layer-location
      ==
  ==
::
+|  %link-chain
::
::  Consensus proof (Nakamoto PoW)
::
+$  consensus-proof
  $%  [%nakamoto nonce=@ud difficulty=@ud]
  ==
::
::  Block structure
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
::  Chain state metadata
::
+$  chain-state
  $:  blocks=(map @uvH block)
      block-height=(map @ud @uvH)
      best-tip=@uvH
      difficulty=@ud
  ==
::
+|  %link-tasks
::
::  Tasks for the %link vane
::
+$  link-task
  $%  [%born ~]
      [%trim ~]
      [%vega ~]
      [%execute-transaction tx=link-transaction]
      [%validate-block block=*]
      [%add-to-mempool tx=link-transaction]
  ==
::
::  Gifts from the %link vane
::
+$  link-gift
  $%  [%transaction-executed result=execution-result]
      [%transaction-failed result=execution-result]
      [%block-validated blk=block]
      [%block-invalidated blk=block]
      [%new-block blk=block]
  ==
::
+|  %myco-types
::
::  Layer registry entry
::
+$  layer-info
  $:  location=layer-location
      controller=@p
      link-state=link-state
      chain-state=chain-state
      mempool=(list link-transaction)
      active=?
  ==
::
::  Cross-layer message
::
+$  layer-message
  $:  from-layer=layer-location
      to-layer=layer-location
      payload=*
  ==
::
::  Myco tasks
::
+$  myco-task
  $%  [%born ~]
      [%trim ~]
      [%vega ~]
      ::  Layer management
      [%create-layer loc=layer-location controller=@p]
      [%destroy-layer loc=layer-location]
      ::  Transaction routing
      [%submit-tx loc=layer-location tx=link-transaction]
      ::  Cross-layer messaging
      [%relay-message msg=layer-message]
      ::  Block production trigger
      [%produce-block loc=layer-location]
      ::  RPC endpoint handling
      [%rpc-request loc=layer-location method=@t params=*]
      ::  Jael integration
      [%sign-tx tx=link-transaction]
      [%verify-id who=@p]
  ==
::
::  Myco gifts
::
+$  myco-gift
  $%  [%layer-created loc=layer-location]
      [%layer-destroyed loc=layer-location]
      [%tx-result loc=layer-location result=execution-result]
      [%block-produced loc=layer-location blk=block]
      [%rpc-response data=*]
      [%message-delivered msg=layer-message]
      [%signed-tx tx=link-transaction]
      [%identity-verified who=@p valid=?]
  ==
--
