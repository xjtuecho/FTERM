
(********************************************************************************)
FTERM(http://fterm.zj001.net) is a telnet/ssh client written by

fuse(fuse@dot66.com)
kxn(kxn@smth.org)

with feature of SSH1/2, Z-modem protocol and many interesting function for bbs.

The Core component of FTERM is TnEmulVT, which is from ICS(Internet Component Suite)
(francois.piette@overbyte.be    http://www.overbyte.be)

Quite a lot enhancement have been done to the origional ICS code.


(********************************************************************************)
TnEmulVT for FTERM

The following enhancement has been made:
  o Chinese Support.
  o Blink, Underline
  o Caret type (ctBLine, ctBlock, ctBeam)
  o Select mode (smLine, smBlock)
  o Parse URL
  o send new windows size 
  o xterm mouse support
  o Linux console sequence mostly support
  o new keyboard scheme
  o better speed and feeling
  o and many more....

Two main feature added:
 *o SSH1/2 support (by kxn@smth.org)
 *o ZModem support.


(********************************************************************************)
For developer.

1. Use Delphi 6.

2. Ensure you don't have ICS installed, compile and install ICSTnx60.dpk.

3. WSocket.pas is modified to add HTTP proxy support. If you don't need this feature
   just forget it and use the main ICS distribution code.

4. To enable SSH feature, edit TnCnx.pas Line:74
   copy ICS/libeay32.dll to your executable path.


(********************************************************************************)
Contact me:

mailto:fuse@dot66.com
If you can read chinese, visit http://fterm.zj001.net


