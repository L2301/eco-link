::  %link: blockchain world state vane
::  Empty implementation that crashes on everything
::
!:
|=  our=ship
=,  link
=>
|%

+|  %helpers
++  execute-transaction
  |=  [state=link-state tx=link-transaction]
  ^-  execution-result
  ?>  ?=(%transfer -.tx)
  =/  from-balance  (~(gut by balances.state) from.tx 0)
  ::  Check sufficient balance
  ?:  (lth from-balance amount.tx)
    [%.n state]  :: Failed, return unchanged state
  ::  Execute transfer
  =/  new-balances
    =.  balances.state  (~(put by balances.state) from.tx (sub from-balance amount.tx))
    =/  to-balance  (~(gut by balances.state) to.tx 0)
    (~(put by balances.state) to.tx (add to-balance amount.tx))
  =/  new-state  state(balances new-balances)
  [%.y new-state]  :: Success, return new state

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
    =/  gift-data=gift:link  [%transaction-result result=executed]
    =/  gift-wrapped  [%& p=[!>(gift-data)]]
    :_  link-gate(state new-state.executed)
    ~
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
