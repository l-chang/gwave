* RC circuit driven by square wave 
* Run with command: SPICE_ASCIIRAWFILE=1 ngspice  -r rcsq.raw -b rcsq.sp

* unlike modern spices, GND and node 0 are not aliases for each other.
vgnd 0 GND 0

R1 A B 50
C1 B GND 100pf

Va A GND PWL (0 0 50n 0 51n 5 100n 5 101n 0 200n 0)

.tran 0.5n 200n
.plot tran v(A) V(B)
.option POST=1 PROBE

.end
