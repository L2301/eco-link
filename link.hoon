::  %link: blockchain world state vane
::  Empty implementation that crashes on everything
::
|=  our=ship
=>
|%
::  Type definitions
::
+$  task  *                                :: TODO: define blockchain tasks
+$  gift  *                                :: TODO: define blockchain gifts  
+$  link-state                             :: TODO: define blockchain state
  $:  balances=(map @p @ud)
  ==
--
::
=|  link-state
|=  [now=@da eny=@uvJ rof=*]                :: simplified roof type
|%
::  Vane interface
::
++  call
  |=  [=duct dud=(unit *) wrapped-task=*]
  ^-  [(list move) _link-gate]
  [~ link-gate]
::
++  take
  |=  [=wire =duct dud=(unit *) sign=*]
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
