-- Data type definition

-- Enumeration
data LightSwitch = On | Off

toggle : LightSwitch -> LightSwitch
toggle On = Off
toggle Off = On

-- Union, basically an email where members can have values
data Shape =
  Triangle Double Double |
  Rectangle Double Double |
  Circle Double

