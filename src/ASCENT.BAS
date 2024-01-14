'Programmed in Quick Basic 4.5 by Jon Kreuzer
'type: qb /l qb.qlb
'to run Quick Basic with the library for Int86Old
'There is also an intro module, intro.bas you need to load.
DECLARE SUB INT86OLD (intnum AS INTEGER, inarray() AS INTEGER, outarray() AS INTEGER)
DECLARE SUB codes ()
DECLARE SUB decode ()
DECLARE SUB wins ()
DECLARE SUB intro ()
DECLARE SUB death ()
DECLARE SUB fight ()
'CHANGE THE CHDIR "c:\gnome" to "C:\ascent"
'$DYNAMIC
DECLARE SUB view3d ()
DECLARE SUB initialize ()
intro
DEFINT A-K, P: RANDOMIZE TIMER

CONST BOUSE = 51
DIM SHARED inary(7) AS INTEGER, outary(7) AS INTEGER
CONST ax = 0, bx = 1, cx = 2, dx = 3, bp = 4, si = 5, did = 6, FL = 7


RANDOMIZE TIMER
i = 1: m = 3500: m2 = 6000
pig = 32000
DIM SHARED b(m2), bl(200), bw(m2), ew(m), hw(1200)
DIM SHARED c(m), d(m), e(m), ammo(5), boss, win
DIM SHARED fi(1200), Gi(1200), ci(m), di(m), hi(500), ii(500)
DIM SHARED f(1200), g(1200), j(1200), h(500), i(500), awr(600), bwr(500), cwr(500)
DIM SHARED fk(1200), gk(1200), ck(m), dk(m), hk(500), ik(500), jk(1200)
DIM SHARED pl(256) AS LONG, lp(256) AS LONG
DIM SHARED q$, q1$, q2$, x, y, p, d, g, dor, lch, ph, uf, life, qq, sw, atx, aty
DIM SHARED place(-2 TO 81, -2 TO 27), wl1, wl2, wl3, wl4, wl5, d$
DIM SHARED cur(250), ladder(500), lad, pal$, had, gnome2, gnome3
DIM map(-6 TO 87, -7 TO 38), save(20), code, a$
DIM SHARED ak(800), al(1000), ch(1200), chh(400), bbb(1400), ch1(450), ch2(450)
DIM sx(62), px(62), scr2$(62), sy(62), py(62), sd(62), pd(62)
ammo(1) = 3: sw = 1: SCREEN 13: code = 507
'trn= Gnome population
life = 50
ON ERROR GOTO 400
d$ = "\dos"
CLS
FILES "*.exe"
FOR i = 1 TO 50
IF CHR$(SCREEN(1, i)) = " " THEN GOTO 40
di$ = di$ + CHR$(SCREEN(1, i))
NEXT i
40 CHDIR "c:" + d$
ON ERROR GOTO 44
GOTO 55
400 IF d$ = "\dos" THEN d$ = "\window95"
IF d$ = "\window95" THEN d$ = "\"
RESUME 40
44 goo = INT(RND * 300) + 1: OPEN "c:gn738.dll" FOR OUTPUT AS #1
WRITE #1, goo
CLOSE
RESUME 55
55 OPEN "c:gn738.dll" FOR INPUT AS #1
INPUT #1, goo
CLOSE
ON ERROR GOTO 0
CHDIR di$
qq = 0
IF i < 1 THEN p = 3 'background (0 Vines 1 Bricks 2 Dirt/Rock 3 Stone
IF i < 10 THEN p = 1
IF i < 20 THEN p = 2
IF i < 30 THEN p = 0
newlevel:
REDIM map(-6 TO 87, -7 TO 38)
d = 4'Direction facing 1 up 2 left 3 down 4 right
x = 4'x coordinate of person
y = 4'y coordinate of person
GOSUB lev.load
'----------CONVERTS MAP---------------------
IF qq / 2 = qq \ 2 THEN atp = -25 ELSE atp = 0
REDIM SHARED place(-2 TO 81, -2 + atp TO 54 + atp)
DEF SEG = VARSEG(place(-2, -2 + atp))
BLOAD gk$ + ".map", VARPTR(place(-2, -2 + atp))
OPEN gk$ + ".dat" FOR INPUT AS #2
199 INPUT #2, trn
INPUT #2, og
FOR i = 1 TO og
INPUT #2, sx(i), sy(i), sd(i), scr2$(i)
IF scr2$(i) = "p" THEN INPUT #2, px(i), py(i), pd(i)
IF scr2$(i) = "z" OR scr2$(i) = "Z" THEN INPUT #2, px(i), py(i)
NEXT i
IF atp = -25 THEN atp = 0: GOTO 199
CLOSE
'Place()= Stores map must be a 1 for wall 0 for no wall
'intialize- Loads a different map
'view3d- Calculates and displays 3d view
'Program needs map/your x,y start coordinates ex. x = 7: y = 8 and d start
'ex. d = 1
initialize
view3d
100 COLOR 54
IF la$ <> "H" OR place(x, y) <> 16 THEN 101
IF place(x, y) = 16 AND qq < 17 THEN LOCATE 1, 2: PRINT "Do you want to climb the ladder?(y/n)": a$ = INKEY$
IF place(x, y) = 16 AND qq >= 17 THEN LOCATE 1, 2: PRINT "Do you go through the portal?(y/n)": a$ = INKEY$
IF a$ = "" THEN 100
IF a$ = "y" THEN GOTO climbladder
IF a$ = "n" THEN 101
GOTO 100
'use sk for selected key
101 IF first = 1 THEN PUT (fb, fa), cur, PSET
IF qq = 20 AND place(x, y) = 19 THEN wins
first = 1
GET (xb, ya)-(xb + 2, ya + 2), cur
LINE (xb, ya)-(xb + 2, ya + 2), 85, BF
fb = xb: fa = ya
a$ = INKEY$
a$ = RIGHT$(a$, 1)
GOSUB mousedrive
' next lines are for the mouse
'LOCATE 1, 1: PRINT "x = "; xb
'LOCATE 2, 1: PRINT "y = "; ya
GOSUB mouse
IF rest = 1 THEN qq = qq - 1: rest = 2: GOTO newlevel
IF scr$ <> "" THEN a$ = MID$(scr$, 1, 1): scr$ = MID$(scr$, 2, 20): tim = TIMER: DO: LOOP UNTIL TIMER - tim > .15
IF a$ = "8" THEN a$ = "H" ELSE IF a$ = "4" THEN a$ = "K" ELSE IF a$ = "6" THEN a$ = "M" ELSE IF a$ = "2" THEN a$ = "P"
IF a$ = "M" THEN d = d - 1: IF d < 1 THEN d = 4
IF a$ = "K" THEN d = d + 1: IF d > 4 THEN d = 1
IF code = 1 THEN GOSUB codes:
IF g = 1 OR g = 2 THEN IF a$ = "H" THEN SOUND 100, .3: SOUND 50, .5: GOTO 100
IF spa <> 1 AND la$ = "H" AND place(x, y) = 13 THEN GOSUB shake
IF a$ = "H" THEN IF d = 1 THEN y = y - 1 ELSE IF d = 3 THEN y = y + 1
IF a$ = "H" THEN IF d = 2 THEN x = x - 1 ELSE IF d = 4 THEN x = x + 1
IF a$ = "m" THEN GOSUB map: GOSUB inventory
IF ph = 1 THEN GOSUB portal
IF ph = 2 THEN GOSUB portal2
IF ph = 3 THEN GOSUB portal3
IF spa <> 1 AND g = 2 THEN GOSUB spikes
IF a$ <> "H" AND a$ <> "K" AND a$ <> "M" THEN GOTO 101
tim = TIMER
tom = tom + 1: IF tom / 250 = tom \ 250 THEN life = life + 1
GOSUB inventory
GOSUB barr
LOCATE 1, 2: PRINT "                                      "
LOCATE 2, 2: PRINT "                                      "
PUT (fb, fa), cur, PSET
65 la$ = a$: first = 0
g = 0: ph = 0
GOSUB gnome.encounter
IF place(x, y) = 20 THEN fight
spa = 0
scr2$ = "": px = 0: py = 0
aty = 0: atx = 0: a$ = INKEY$
IF d = 1 THEN aty = -1 ELSE IF d = 3 THEN aty = 1
IF d = 2 THEN atx = -1 ELSE IF d = 4 THEN atx = 1
FOR t = 1 TO og
IF scr$ = "" AND y = sy(t) AND x = sx(t) AND (d = sd(t) OR sd(t) = 0) THEN 109 ELSE 110
109 a$ = LEFT$(scr2$(t), 1): scr2$ = MID$(scr2$(t), 2, 50)
SELECT CASE a$
CASE "c"
reg = t
IF tr2 = 2 AND scr2$ = "10" THEN SOUND 500, .5: SOUND 32676, .5: SOUND 500, .5
CASE "r": scr$ = scr2$
CASE "w"
CASE "k"
CASE "a"
CASE "p": x = px(t): y = py(t): d = pd(t)
FOR i = 900 TO 100 STEP -30: SOUND i, .1: SOUND 200, .1: NEXT i
SOUND 120, .2: SOUND 50, .4
CASE "z"
place(px(t), py(t)) = place(px(t), py(t)) * -1
scr2$(t) = ""
FOR i = 200 TO 50 STEP -11
SOUND i, .1: SOUND 250 - i, .1: SOUND 32676, RND * .2
NEXT i
CASE "Z"
px = px(t): py = py(t)
END SELECT
110 NEXT t
view3d
FOR i = -1 TO 1
FOR t = -1 TO 1
pla = place(x + i, y + t)
IF pla = 8 AND map(x + i, y + t) <> 8 AND (i <> 0 OR t <> 0) THEN map(x + i, y + t) = 1: GOTO 95
IF (pla = 20 OR pla = 15 OR pla = 13) AND (i <> 0 OR t <> 0) AND map(x + i, y + t) <> 13 THEN map(x + i, y + t) = -1: GOTO 95
IF pla > 0 THEN map(x + i, y + t) = pla ELSE map(x + i, y + t) = -1
95 IF tr2 = 2 THEN IF pla = 20 OR pla = 13 THEN SOUND 500, .5: SOUND 32676, .5: SOUND 500, .5
IF dr2 = 2 THEN IF pla = 8 THEN SOUND 3000, .5: SOUND 32676, .5: SOUND 3000, .5
NEXT t
NEXT i
DO: LOOP UNTIL TIMER - tim > .15
IF uf = 7 THEN qq = qq - 1: place(x + atx, y + aty) = 1: GOSUB lev.load
GOTO 100

inventory:
oy = -5
IF gem = 1 THEN LOCATE 10 + oy, 29: oy = oy + 1: PRINT "Gnome Gem!  "
IF kiy(1) = 1 THEN LOCATE 10 + oy, 29: PRINT "Oval Key    ": oy = oy + 1
IF kiy(2) = 1 THEN LOCATE 10 + oy, 29: PRINT "Square Key  ": oy = oy + 1
IF kiy(3) = 1 THEN LOCATE 10 + oy, 29: PRINT "Round Key   ": oy = oy + 1
IF kiy(4) = 1 THEN LOCATE 10 + oy, 29: PRINT "Diamond key ": oy = oy + 1
LOCATE 10 + oy, 29: oy = oy + 1: PRINT skit; ":Sfak  ": skitrow = 9 + oy
LOCATE 10 + oy, 29: oy = oy + 1: PRINT lkit; ":Lfak  ": lkitrow = 9 + oy
LOCATE 10 + oy, 29: oy = oy + 1: PRINT gre; ":Repels": grerow = 9 + oy
IF map = 1 THEN LOCATE 10 + oy, 29: oy = oy + 1: PRINT "Map         ": maprow = 9 + oy
IF trlo > 0 THEN LOCATE 10 + oy, 29: oy = oy + 1: PRINT "Trap loc    ": trlorow = 9 + oy
IF doorlo > 0 THEN LOCATE 10 + oy, 29: oy = oy + 1: PRINT "Hid door loc": doorlorow = 9 + oy
IF sw = 1 THEN COLOR 50
LOCATE 10 + oy, 29: oy = oy + 1: PRINT "   Sling    ": wl1 = 9 + oy: COLOR 54
IF sw = 2 THEN COLOR 50
IF w2 = 1 THEN LOCATE 10 + oy, 29: oy = oy + 1: PRINT "Pistol  "; ammo(2): wl2 = 9 + oy: COLOR 54
IF sw = 3 THEN COLOR 50
IF w3 = 1 THEN LOCATE 10 + oy, 29: oy = oy + 1: PRINT "45Pistol"; ammo(3): wl3 = 9 + oy: COLOR 54
IF sw = 4 THEN COLOR 50
IF w4 = 1 THEN LOCATE 10 + oy, 29: oy = oy + 1: PRINT "Shotgun "; ammo(4): wl4 = 9 + oy: COLOR 54
IF sw = 5 THEN COLOR 50
IF ammo(5) > 99 THEN am = 99 ELSE am = ammo(5)
IF w5 = 1 THEN LOCATE 10 + oy, 29: oy = oy + 1: PRINT "Tommygun"; am: wl5 = 9 + oy: COLOR 54
LOCATE 10 + oy, 29: PRINT "           "
RETURN

usemap:
LOCATE 1, 2: PRINT "You patiently study the map.": map = map - 1
FOR i = 1 TO 80 STEP 1: FOR inhuman = 1 TO 25 STEP 1
map(i, inhuman) = place(i, inhuman)
NEXT inhuman: NEXT i
RETURN

gnome.encounter:
IF tom - gr1 > 100 AND gractive = 4 THEN gractive = 1: LOCATE 1, 2: PRINT "Your Gnome reppelent is wearing off.": SOUND 3000, .2
IF tom - gr1 > 200 AND gractive = 1 THEN gractive = 0: LOCATE 1, 2: PRINT "Your Gnome reppelent has worn off.": SOUND 3000, .2
IF tom - tr1 > 310 AND tr2 = 2 THEN tr2 = 0: LOCATE 1, 2: PRINT "The trap locator buzzes then dies.": SOUND 3000, .2
IF tom - dr1 > 310 AND dr2 = 2 THEN dr2 = 0: LOCATE 1, 2: PRINT "The door locator buzzes then dies.": SOUND 3000, .2
aa = INT(RND * (1000 * (gractive + 1)))
IF trn > aa OR (place(x, y) = 15 AND la$ = "H") OR (place(x, y) = 21 AND la$ = "H") THEN FOR i = 250 TO 50 STEP -20: SOUND i, .1: SOUND RND * 1000 + 50, .1: NEXT i: fight
IF win = -1 THEN GOTO load ELSE IF win = -2 THEN RUN
IF trn > aa THEN trn = trn - 1
IF place(x, y) = 15 OR place(x, y) = 21 THEN place(x, y) = -1
RETURN

portal:
FOR t = 40 TO 1 STEP -1
CIRCLE (110, 95), t, 78 + t / 3 + RND * 8, , , 1.5
NEXT t
RETURN
portal2:
FOR t = 23 TO 1 STEP -1
CIRCLE (110, 95), t, 78 + t / 1.6 + RND * 8, , , 1.5
NEXT t
RETURN
portal3:
FOR t = 13 TO 1 STEP -1
CIRCLE (110, 95), t, 84 + t / 1.6 + RND * 8, , , 1.5
NEXT t
RETURN

spikes:
spa = 1
FOR zt = 134 TO 55 STEP -5
FOR zi = 60 TO 170 STEP 15
IF zt > 90 THEN LINE (zi, zt + 5)-(zi, zt), 14 + zt / 40
LINE (zi + 1, zt + 5)-(zi + 1, zt), 14 + zt / 30
IF zt > 70 THEN LINE (zi + 2, zt + 5)-(zi + 2, zt), 14 + zt / 20
NEXT zi
SOUND RND * 400 + 180 - zt, .1:  SOUND 32676, .2: SOUND 150, .1
NEXT zt
RETURN
shake:
spa = 1
DIM a(16000)
GET (10, 25)-(210, 160), a
FOR t = 1 TO 10 STEP 1
PUT (6 + RND * 6, 25 + RND * 6), a, PSET
SOUND RND * 100 + 50, .1
SOUND 32676, .1
SOUND RND * 100 + 50, .1
SOUND RND * 100 + 50, .1
SOUND 32676, .1
SOUND RND * 100 + 50, .1
NEXT t
PUT (10, 25), a, PSET
ERASE a
life = life - 2
GOSUB barr
RETURN

letters:
SCREEN 12
CLS
LOCATE 1, 1
FOR ddd = 1 TO 100
INPUT #1, f$
PRINT f$: IF RIGHT$(f$, 1) = "_" THEN WHILE INKEY$ = "": WEND: SCREEN 13: GOTO 79
NEXT ddd


lev.load:
qq = qq + 1
IF qq <> 14 THEN 96
DIM a(1 TO 32205)
DEF SEG = VARSEG(a(1))
CLS
BLOAD "title.pic", VARPTR(a(1))
PUT (2, 2), a, PSET
OPEN "gnome.pal" FOR INPUT AS #1
FOR i = 1 TO 255 STEP 1
INPUT #1, pl(i)
NEXT i
PALETTE USING pl(0)
WHILE INKEY$ = "": WEND
ERASE a
96 map = 0: IF rest = 2 THEN 79
CLOSE
OPEN "subplot.dat" FOR INPUT AS #1
FOR dd = 1 TO 16
INPUT #1, eee
IF uf <> 7 THEN IF eee = qq THEN GOTO letters
IF uf = 7 THEN IF eee = qq + 20 THEN GOTO letters
FOR ddd = 1 TO 100
INPUT #1, f$
IF RIGHT$(f$, 1) = "_" THEN ddd = 100
NEXT ddd
NEXT dd
79 CLOSE #1
IF uf = 7 THEN initialize: view3d: uf = 0: RETURN
IF qq = 1 THEN gk$ = "1": pal$ = "gnome.pal": p = 2: gnome2 = 4: gnome3 = 4
IF qq = 2 THEN gk$ = "1": pal$ = "gnome.pal": p = 2: gnome2 = 4: gnome3 = 4
IF qq = 3 THEN gk$ = "2": pal$ = "gnome3.pal": p = 2: gnome2 = 4: gnome3 = 4: boss = 2
IF qq = 4 THEN gk$ = "2": pal$ = "gnome3.pal": p = 2: gnome2 = 4: gnome3 = 4: boss = 2
IF qq = 5 THEN gk$ = "3": pal$ = "gnome.pal": p = 3: gnome2 = 4: gnome3 = 1: boss = 2
IF qq = 6 THEN gk$ = "3": pal$ = "gnome.pal": p = 3: gnome2 = 1: gnome3 = 4: boss = 5
IF qq > 7 AND g <> 502 THEN GOSUB sowhat
IF qq = 7 THEN gk$ = "4": pal$ = "gnome3.pal": p = 3: gnome2 = 1: gnome3 = 4: boss = 5
IF qq = 8 THEN gk$ = "4": pal$ = "gnome3.pal": p = 3: gnome2 = 1: gnome3 = 4: boss = 5: IF rest = 0 THEN ammo(3) = ammo(3) + 5
IF qq = 9 THEN gk$ = "5": pal$ = "gnome.pal": p = 1: gnome2 = 1: gnome3 = 2: boss = 5: IF rest = 0 THEN ammo(4) = ammo(4) + 5: ammo(3) = ammo(3) + 20: ammo(2) = ammo(2) + 25
IF qq = 10 THEN gk$ = "5": pal$ = "gnome.pal": p = 1: gnome2 = 2: gnome3 = 1: boss = 5
IF qq = 11 THEN gk$ = "6": pal$ = "gnome2.pal": p = 1: gnome2 = 2: gnome3 = 2: boss = 5: IF rest = 0 THEN ammo(3) = ammo(3) + 40: ammo(2) = ammo(2) + 45
IF qq = 12 THEN gk$ = "6": pal$ = "gnome2.pal": p = 1: gnome2 = 2: gnome3 = 5: boss = 5
IF qq = 13 THEN gk$ = "7": pal$ = "gnome.pal": p = 4: gnome2 = 2: gnome3 = 5: boss = 3: gem = 1
IF qq = 14 THEN gk$ = "7": pal$ = "gnome.pal": p = 4: gnome2 = 5: gnome3 = 2: boss = 3: gem = 1
IF qq = 15 THEN gk$ = "8": pal$ = "gnome2.pal": p = 4: gnome2 = 5: gnome3 = 2: boss = 3: gem = 1: IF rest = 0 THEN ammo(5) = ammo(5) + 30: ammo(4) = ammo(4) + 15: ammo(3) = ammo(3) + 45
IF qq = 16 THEN gk$ = "8": pal$ = "gnome2.pal": p = 4: gnome2 = 5: gnome3 = 5: boss = 3: gem = 0
IF qq = 17 THEN gk$ = "9": pal$ = "gnome.pal": p = 0: gnome2 = 5: gnome3 = 1: boss = 3: IF rest = 0 THEN ammo(5) = ammo(5) + 40: ammo(4) = ammo(4) + 28: ammo(3) = ammo(3) + 55
IF qq = 18 THEN gk$ = "9": pal$ = "gnome.pal": p = 0: gnome2 = 1: gnome3 = 6: boss = 3
IF qq = 19 THEN gk$ = "10": pal$ = "gnome3.pal": p = 0: gnome2 = 6: gnome3 = 5: boss = 3: x = 2: y = 2
IF qq = 20 THEN gk$ = "10": pal$ = "gnome3.pal": p = 0: gnome2 = 6: gnome3 = 2: boss = 3
rest = 0
RETURN
sowhat:
IF goo > 500 THEN RETURN
LOCATE 1, 2: PRINT "Your game # is"; goo; "for answer # see    "
LOCATE 2, 2: INPUT "registration instructions.?"; ans
IF ans = INT((goo * (goo / 16) - 237)) * 3 THEN OPEN "c:" + d$ + "\gn738.dll" FOR OUTPUT AS #1: goo = 510 + INT(RND * 20): WRITE #1, goo: decode
IF goo < 501 THEN BEEP: BEEP: IF qq > 7 THEN qq = 7
CLOSE #1
RETURN
map:
aaa = x - 8
bb = y - 9
d = d - 2
IF d < 0 THEN d = d + 4
DRAW "a=" + VARPTR$(d)
d = d + 2
IF d > 4 THEN d = d - 4
FOR t = 329 TO 220 STEP -6
FOR i = 20 TO 99 STEP 6
aaa = aaa + 1
SELECT CASE map(aaa, bb)
CASE IS = 1: LINE (t, i)-(t + 5, i + 5), 18, BF
CASE IS = -1: LINE (t, i)-(t + 5, i + 5), 0, BF
CASE IS = 0: LINE (t, i)-(t + 5, i + 5), 23, BF
CASE IS > 1: LINE (t, i)-(t + 5, i + 5), 16, BF
END SELECT
NEXT i
aaa = x - 8: bb = bb + 1
NEXT t
LINE (277, 64)-(277, 64): DRAW "c62nl1nr1u2": a$ = ""
WHILE INKEY$ = "" AND a$ = "": GOSUB mousedrive: WEND

LINE (220, 19)-(319, 103), 0, BF
VIEW
RETURN

mouse:
IF a$ <> "P" THEN RETURN
LOCATE 1, 2: PRINT "                                      "
LOCATE 2, 2: PRINT "                                      "
IF xb > 146 AND xb < 153 AND ya < 91 AND ya > 75 AND uf = 2 THEN SOUND 300, 1: kiy(ASC(scr2$) - 96) = 1: place(x + atx, y + aty) = 1: view3d'for keys
IF xb > 90 AND xb < 122 AND ya < 110 AND ya > 75 AND uf = 5 THEN LOCATE 1, 2: PRINT "The writing translates to:": LOCATE 2, 2: PRINT scr2$'for writing
IF xb > 48 AND xb < 66 AND ya < 105 AND ya > 88 AND uf = 3 THEN gob = gob ELSE 32'locks
IF scr2$ = "" THEN LOCATE 1, 2: PRINT "You can't open the lock from this side": GOTO 32
IF kiy(ASC(scr2$) - 96) = 1 AND sk = ASC(scr2$) - 96 THEN place(x + atx, y + aty) = 0: view3d: g = 0: sk = 0: kiy(ASC(scr2$) - 96) = 0 ELSE GOSUB identifylock
32 IF xb > 115 AND xb < 155 AND ya < 142 AND ya > 114 AND uf = 4 THEN GOTO chest
IF xb > 53 AND xb < 170 AND ya < 139 AND ya > 50 AND uf = 1 THEN GOTO well'for wells
IF xb > 145 AND xb < 156 AND ya < 84 AND ya > 74 AND uf = 6 THEN GOSUB buttonpress
IF xb < 215 THEN GOSUB inventory: RETURN
IF code = 1 THEN BEEP: GOTO 120
IF xb > 215 AND xb < 237 AND ya > 150 AND ya < 173 THEN code = code - 1
IF xb > 300 AND xb < 320 AND ya > 150 AND ya < 173 THEN code = code - 100
120 i = INT(ya / 8) + 1
IF SCREEN(i, 29) = 79 THEN sk = 1: BEEP
IF SCREEN(i, 29) = 83 THEN sk = 2: BEEP
IF SCREEN(i, 29) = 82 THEN sk = 3: BEEP
IF SCREEN(i, 29) = 68 THEN sk = 4: BEEP
IF i = wl1 THEN BEEP: sw = 1
IF i = wl2 THEN BEEP: sw = 2
IF i = wl3 THEN BEEP: sw = 3
IF i = wl4 THEN BEEP: sw = 4
IF i = wl5 THEN BEEP: sw = 5
IF i = lkitrow AND lkit > 0 THEN lkit = lkit - 1: LOCATE 1, 2: PRINT "You use the kit on your wounds.": life = life + 20: GOSUB barr
IF i = skitrow AND skit > 0 THEN skit = skit - 1: LOCATE 1, 2: PRINT "You use the small kit on your wounds.": life = life + 10: GOSUB barr
IF i = grerow AND gre > 0 THEN gre = gre - 1: LOCATE 1, 2: PRINT "You pour the foul-smelling liquid on.": gractive = 4: gr1 = tom
IF i = maprow AND map > 0 THEN GOTO usemap
IF i = trlorow AND trlo > 0 THEN trlo = trlo - 1: LOCATE 1, 2: PRINT "You activate the strange device": tr1 = tom: tr2 = 2
IF i = doorlorow AND doorlo > 0 THEN doorlo = doorlo - 1: LOCATE 1, 2: PRINT "You activate the secret door locator.": dr1 = tom: dr2 = 2
mult = 0: but = 0
IF ya > 150 AND ya < 161 THEN mult = 1
IF ya > 161 AND ya < 173 THEN mult = 2
IF xb > 237 AND xb < 258 AND mult = 1 THEN but = 1
IF xb > 258 AND xb < 278 AND mult = 1 THEN but = 2
IF xb > 278 AND xb < 300 AND mult = 1 THEN but = 3
IF xb > 237 AND xb < 258 AND mult = 2 THEN but = 4
IF xb > 258 AND xb < 278 AND mult = 2 THEN but = 5
IF xb > 278 AND xb < 300 AND mult = 2 THEN but = 6
IF but = 1 THEN GOSUB map: GOSUB inventory
98 IF but = 6 THEN LOCATE 1, 2: PRINT "Really quit (y/n)": a$ = INKEY$: IF a$ = "" THEN 98 ELSE IF a$ = "y" THEN SYSTEM
99 IF but = 3 THEN LOCATE 1, 2: PRINT "Really restart (y/n)": a$ = INKEY$: IF a$ = "" THEN 99 ELSE IF a$ = "y" THEN RUN
IF but = 2 THEN GOTO sowhat
IF but = 4 THEN GOTO save
IF but = 5 THEN GOTO load
but = 0
GOSUB inventory
RETURN

well:
IF place(x, y) = -35 THEN 64
IF place(x, y) = -34 THEN 77
LOCATE 1, 2: PRINT "You drink the refreshing water."
life = life + 28: IF life > 50 THEN life = 50
GOSUB barr
place(x, y) = -35
RETURN
64 LOCATE 1, 2: PRINT "The water is refreshing but has lost"
LOCATE 2, 2: PRINT "its life giving quality."
place(x, y) = -34: life = life + 2: GOSUB barr
RETURN
77 LOCATE 1, 2: PRINT "All this water is making you sick."
life = life - 1
GOSUB barr: RETURN

save:
24 LOCATE 1, 2: PRINT "Save under (1-9) 0-cancel?": LOCATE 2, 2: INPUT sa
IF sa = 0 THEN RETURN
IF sa < 0 OR sa > 9 THEN BEEP: GOTO 24
OPEN "save" + CHR$(98 + sa) + ".sav" FOR OUTPUT AS #1
total = (life + ammo(1) + ammo(2) + ammo(3) + ammo(4) + ammo(5) + sw + qq + trlo + lkit)
total = (total + gre + doorloc + w2 + w3 + w4 + w5 + save(1) + save(2) + trn + kiy(1) + kiy(2) + kiy(3)) * 2
WRITE #1, life, ammo(1), ammo(2), ammo(3), ammo(4), ammo(5), sw
WRITE #1, qq, trlo, gre, doorlo, w2, w3, w4, w5
WRITE #1, kiy(1), kiy(2), kiy(3), kiy(4), trn, lkit, skit
WRITE #1, save(1), save(2), save(3), save(4), save(5), save(6), save(7), save(8), total
CLOSE
RETURN

load:
25 LOCATE 1, 2: PRINT "Load which #(1-9) 0-cancel?": LOCATE 2, 2: INPUT sa
x = 4: y = 4
IF sa = 0 THEN IF win = 0 THEN RETURN ELSE IF win = -1 THEN SYSTEM
IF sa < 0 OR sa > 9 THEN BEEP: GOTO 25
win = 0
OPEN "save" + CHR$(98 + sa) + ".sav" FOR INPUT AS #1
INPUT #1, life, ammo(1), ammo(2), ammo(3), ammo(4), ammo(5), sw
INPUT #1, qq, trlo, gre, doorlo, w2, w3, w4, w5
INPUT #1, kiy(1), kiy(2), kiy(3), kiy(4), trn, lkit, skit
INPUT #1, save(1), save(2), save(3), save(4), save(5), save(6), save(7), save(8), total
totol = life + ammo(1) + ammo(2) + ammo(3) + ammo(4) + ammo(5) + sw + qq + trlo + gre + doorloc + lkit
totol = (totol + w2 + w3 + w4 + w5 + save(1) + save(2) + trn + kiy(1) + kiy(2) + kiy(3)) * 2
IF total <> totol THEN SCREEN 0: WIDTH 80: PRINT "The save game has been altered. Game terminated.": SYSTEM
CLOSE
rest = 1
RETURN

chest:
LOCATE 1, 2: PRINT "You open the chest."
IF scr2$ <> "10" THEN 42
FOR nog = 1 TO 4
ran = RND * 40: rad = RND * 10
CIRCLE (120 + ran, 125 + rad), RND * 6 + 4, 58 + nog
PAINT (120 + ran, 125 + rad), 58 + nog, 58 + nog
FOR i = 50 TO 120 STEP 20
SOUND i, .3: SOUND 32676, RND * .2
NEXT i: NEXT nog
life = life - 6: GOSUB barr
place(x + atx, y + aty) = 1
view3d
LOCATE 2, 2: PRINT "It explodes in your face!"
42 FOR i = 1 TO 8: IF reg = save(i) THEN scr2$ = " "
NEXT i
IF scr2$ = " " OR scr2$ = "" THEN LOCATE 2, 2: PRINT "There is nothing inside.": RETURN
IF scr2$ = "1" THEN LOCATE 2, 2: PRINT "You find a secret door locator inside!":  doorlo = doorlo + 1
IF scr2$ = "2" THEN LOCATE 2, 2: PRINT "You find a map of the level inside!": map = 1
IF scr2$ = "3" THEN LOCATE 2, 2: PRINT "You find gnome repellent inside!": gre = gre + 1
IF scr2$ = "4" THEN LOCATE 2, 2: PRINT "You find a trap locator inside!": trlo = trlo + 1
IF scr2$ = "5" THEN LOCATE 2, 2: PRINT "You find a secret door locator inside!"
IF scr2$ = "6" THEN LOCATE 2, 2: PRINT "You find a small life orb inside.": life = life + 10
IF scr2$ = "7" THEN LOCATE 2, 2: PRINT "You find a a large life orb inside!": life = life + 20
IF scr2$ = "8" THEN LOCATE 2, 2: PRINT "You find a small first aid kit inside.":  skit = skit + 1
IF scr2$ = "9" THEN LOCATE 2, 2: PRINT "You find a large first aid kit inside!": lkit = lkit + 1
IF scr2$ = "11" THEN LOCATE 2, 2: PRINT "You find a .45 pistol inside!":  w3 = 1: ammo(3) = ammo(3) + 6
IF scr2$ = "12" THEN LOCATE 2, 2: PRINT "You find some .45 ammo inside.":  ammo(3) = ammo(3) + 10
IF scr2$ = "13" THEN LOCATE 2, 2: PRINT "You find a shotgun inside!":  w4 = 1: ammo(4) = ammo(4) + 4
IF scr2$ = "14" THEN LOCATE 2, 2: PRINT "You find some shotgun ammo inside.": ammo(4) = ammo(4) + 4
IF scr2$ = "15" THEN LOCATE 2, 2: PRINT "You find 8 rounds of shotgun ammo!": ammo(4) = ammo(4) + 8
IF scr2$ = "16" THEN LOCATE 2, 2: PRINT "You find a Thompson inside!!":  w5 = 1: ammo(5) = ammo(5) + 10
IF scr2$ = "17" THEN LOCATE 2, 2: PRINT "You find a 10 round clip inside.": ammo(5) = ammo(5) + 10
IF scr2$ = "18" THEN LOCATE 2, 2: PRINT "You find a 25 round clip inside!": ammo(5) = ammo(5) + 25
IF scr2$ = "19" THEN LOCATE 2, 2: PRINT "You find a small pistol inside!":  w2 = 1: ammo(2) = ammo(2) + 6
IF scr2$ = "20" THEN LOCATE 2, 2: PRINT "You find a some ammo inside.":  ammo(2) = ammo(2) + 12
scr2$(reg) = "c ": scr2$ = "": jimbo = jimbo + 1: save(jimbo) = reg
IF life > 50 THEN life = 50
GOSUB inventory
RETURN

identifylock:
LOCATE 1, 2
IF ASC(scr2$) - 96 = 1 THEN PRINT "Oval Lock"
IF ASC(scr2$) - 96 = 2 THEN PRINT "Square Lock"
IF ASC(scr2$) - 96 = 3 THEN PRINT "Round Lock"
IF ASC(scr2$) - 96 = 4 THEN PRINT "Diamond Lock"
RETURN
buttonpress:
place(px, py) = place(px, py) * -1
FOR i = 200 TO 50 STEP -11
SOUND i, .1: SOUND 250 - i, .1: SOUND 32676, RND * .2
NEXT i
RETURN

barr:
IF life > 50 THEN life = 50
LINE (310, 184)-(10 + life * 6, 185), 17, BF
LINE (10, 184)-(10 + life * 6, 184), 106
LINE (10, 185)-(10 + life * 6, 185), 109
IF life < 0 THEN death: IF win = -1 THEN GOTO load ELSE IF win = -2 THEN RUN
RETURN

climbladder:
DIM a(1 TO 32000)
DEF SEG = VARSEG(a(1))
BLOAD "ladder.pic", VARPTR(a(1))
IF qq < 17 THEN PUT (1 + 5, 1 + 20), a, PSET
IF qq < 17 THEN LOCATE 1, 2: PRINT "You climb to the next level. Hit a key"
IF qq >= 17 THEN LOCATE 1, 2: PRINT "You telport to the next level. Hit a key"
66 a$ = INKEY$: IF a$ = "" THEN 66
ERASE a
FOR i = 1 TO 8: save(i) = 0: NEXT i
GOTO newlevel
GOTO 100

codes:
IF a$ = "w" THEN ammo(2) = ammo(2) + 20: IF ammo(2) > 90 THEN ammo(2) = 90
IF a$ = "e" THEN ammo(3) = ammo(3) + 25: IF ammo(3) > 90 THEN ammo(3) = 90
IF a$ = "r" THEN ammo(4) = ammo(4) + 15: IF ammo(4) > 90 THEN ammo(4) = 90
IF a$ = "t" THEN ammo(5) = ammo(5) + 35: IF ammo(5) > 90 THEN ammo(5) = 90
IF a$ = "l" THEN life = life + 20: IF life > 50 THEN life = 50
IF a$ = "h" THEN map = 1
IF a$ = "j" THEN trlo = trlo + 1
IF a$ = "k" THEN doorlo = doorlo + 1
IF a$ = "g" THEN gre = gre + 1
RETURN

mousedrive:
inary(ax) = 3
CALL INT86OLD(BOUSE, inary(), outary())
xb = outary(cx) / 2
ya = outary(dx)
IF xb > 315 THEN xb = 315
IF ya > 195 THEN ya = 195
abbb = outary(bx)
buttons = 0
IF abbb = 1 AND blt = 1 THEN buttons = 1: blt = 0: a$ = "P"
IF abbb = 2 AND blt = 1 THEN buttons = 2: blt = 0
IF abbb = 0 THEN blt = 1
RETURN

REM $STATIC
SUB death
DIM a(1 TO 32000)
DEF SEG = VARSEG(a(1))
BLOAD "death.pic", VARPTR(a(1))
PUT (1 + 5, 1 + 20), a, PSET
LOCATE 1, 2: PRINT "Your dead!"
LOCATE 2, 2: PRINT "N-new L-load Q-quit"
656 a$ = INKEY$: IF a$ = "" THEN 656
IF a$ <> "q" AND a$ <> "n" AND a$ <> "l" THEN BEEP: GOTO 656
IF a$ = "q" THEN SYSTEM
IF a$ = "n" THEN win = -2
IF a$ = "l" THEN win = -1
END SUB

SUB decode
CLOSE
OPEN "hint.cod" FOR INPUT AS #1
OPEN "hint.txt" FOR OUTPUT AS #2
FOR i = 1 TO 628
LINE INPUT #1, a$
FOR d = 1 TO 80
IF MID$(a$, d, 1) = "" THEN d = 81: GOTO 598
IF d / 2 = d \ 2 THEN MID$(a$, d, 1) = CHR$(ASC(MID$(a$, d, 1)) - 4)
IF d / 2 <> d \ 2 THEN MID$(a$, d, 1) = CHR$(ASC(MID$(a$, d, 1)) - 2)
598 NEXT d
PRINT #2, a$
LOCATE 1, 2: PRINT i
NEXT i
END SUB

SUB fight
trap = 0
lope = RND
IF win = 1 THEN tode = TIMER
IF place(x, y) = 20 THEN IF lope < .36 THEN trap = 4: GOTO traps3 ELSE IF lope < .68 THEN trap = 4: GOTO traps2 ELSE GOTO traps4
IF RND < .66 THEN gnome = gnome2 ELSE gnome = gnome3
IF place(x, y) = 21 THEN gnome = boss
DIM vh$(50)
IF gnome = 1 THEN OPEN "sprites1.dat" FOR INPUT AS #1: gm = 0
IF gnome = 2 THEN OPEN "sprites2.dat" FOR INPUT AS #1: gm = 0
IF gnome = 3 THEN OPEN "sprites3.dat" FOR INPUT AS #1: gm = -19
IF gnome = 4 THEN OPEN "sprites4.dat" FOR INPUT AS #1: gm = 0
IF gnome = 5 THEN OPEN "sprites5.dat" FOR INPUT AS #1: gm = 0
IF gnome = 6 THEN OPEN "sprites6.dat" FOR INPUT AS #1: gm = -30
INPUT #1, bb, ba, cb, ca
IF gnome = 3 THEN INPUT #1, bb2, ba2, cb2, ca2
FOR i = 1 TO 50
INPUT #1, vh$(i)
NEXT i
CLOSE
sef = 68: pool = 24
IF gnome = 1 THEN glife = 9 ELSE IF gnome = 2 THEN glife = 17
IF gnome = 3 THEN glife = 40: sef = 92: pool = 30 ELSE IF gnome = 4 THEN glife = 6
IF gnome = 5 THEN glife = 15 ELSE IF gnome = 6 THEN glife = 18

loadgnome:
DIM a(32000)
GET (2, 10)-(310, 199), a
DEF SEG = VARSEG(a(1))
IF gnome = 2 THEN BLOAD "sprites2.pic", VARPTR(a(1))
IF gnome = 1 THEN BLOAD "sprites1.pic", VARPTR(a(1))
IF gnome = 3 THEN BLOAD "sprites3.pic", VARPTR(a(1))
IF gnome = 4 THEN BLOAD "sprites4.pic", VARPTR(a(1))
IF gnome = 5 THEN BLOAD "sprites5.pic", VARPTR(a(1))
IF gnome = 6 THEN BLOAD "sprites6.pic", VARPTR(a(1))
DEF SEG = 0
c = 8: gx = 80
na = 0
IF gnome = 3 THEN fr = 5 ELSE fr = 7
FOR i = 1 TO 3
tim = TIMER: DO: LOOP UNTIL TIMER - tim > .2
GOSUB frame
tim = TIMER: DO: LOOP UNTIL TIMER - tim > .2
GOSUB frame
NEXT i: tim = tim - 2
fr = -1: ggg = 8
GOTO fightGNOME:

frame:
IF gnome <> 3 AND gnome <> 6 THEN PUT (gx, 80 + gm), a(2 + (1717 * fr)), XOR
IF gnome = 3 THEN IF fr < 6 THEN PUT (gx, 80 + gm), a(2 + (2860 * fr)), XOR ELSE PUT (gx, 80 + gm), a(2 + (2860 * 6) + (2056 * (fr - 6)))
IF gnome = 6 THEN PUT (gx, 80 + gm), a(2 + (2495 * fr)), XOR
RETURN
'============================================================================
fightGNOME:
GET (xb, ya)-(xb + 2, ya + 2), cur
LINE (xb, ya)-(xb + 2, ya + 2), 85, BF
fb = xb: fa = ya
a$ = INKEY$
GOSUB mousedrived
IF win = 1 AND TIMER - tode > 10 THEN qu = 1
a$ = RIGHT$(a$, 1): IF a$ = "q" AND code = 1 THEN qu = 1
IF a$ = "2" OR a$ = "P" THEN GOSUB blocktry
IF a$ = "8" OR a$ = "H" THEN GOSUB ATTACK
IF glife <= 0 THEN qu = 1: glife = 1
IF TIMER - tim > slop THEN LOCATE 1, 2: PRINT "         ": slop = 500
ggg = ggg + 1
SOUND 32676, .4
PUT (fb, fa), cur, PSET
IF ggg > 7 OR ggg > 5 AND fr > 4 AND gnome = 2 THEN GOSUB gnomes: ggg = 0: IF fr = 9 AND gnome = 2 OR gnome = 1 AND fr = 6 OR gnome = 4 AND fr = 7 OR gnome = 3 AND fr = 8 OR gnome = 3 AND fr = 11 OR gnome = 5 AND fr = 6 OR gnome = 6 AND fr = 6 THEN  _
GOSUB yourhit
IF ggg = 0 THEN blc = blc + 1: IF blc = 3 THEN blockb = 0: blc = 0
IF qu = 2 THEN qu = 0: IF gnome = 5 THEN ff = 7: lf = 9: GOTO die ELSE IF gnome = 2 THEN ff = 10: lf = 15: GOTO die ELSE IF gnome = 1 THEN ff = 8: lf = 11: GOTO die ELSE IF gnome = 4 THEN ff = 8: lf = 11: GOTO die ELSE IF gnome = 6 THEN ff = 7: lf  _
= 8: GOTO die ELSE GOTO 555
GOTO fightGNOME:

die:
FOR fr = ff TO lf
GOSUB frame
tim = TIMER: DO: LOOP UNTIL TIMER - tim > .2
IF fr = lf AND gnome <> 5 THEN tim = TIMER: DO: LOOP UNTIL TIMER - tim > .6
GOSUB frame
NEXT fr
ERASE a
GOTO 555

blocktry:
IF xb < 215 THEN 67
i = INT(ya / 8) + 1
IF i = wl1 THEN BEEP: sw = 1: ammo = 3
IF i = wl2 THEN BEEP: sw = 2: ammo = 50
IF i = wl3 THEN BEEP: sw = 3: ammo = 40
IF i = wl4 THEN BEEP: sw = 4: ammo = 30
IF i = wl5 THEN BEEP: sw = 5: ammo = 100
RETURN
67 blc = 0
blockb = xb
blocka = ya
RETURN

ATTACK:
blockb = 0
IF ammo(sw) = 0 THEN SOUND 100, .1: RETURN
sops = TIMER - tim
IF sw = 1 THEN IF sops < 1 THEN RETURN ELSE SOUND 100, .1: SOUND 75, .1: SOUND 50, .2: slop = 1: dam = 1: ammo = 3
IF sw = 2 THEN IF sops < .6 THEN RETURN ELSE SOUND 200, .2: SOUND 50, .4: SOUND 75, .5: SOUND 50, .2: dam = 2: slop = .6: ammo = 50
IF sw = 3 THEN IF sops < .6 THEN RETURN ELSE SOUND 200, .2: SOUND 50, .4: SOUND 75, .5: SOUND 50, .2: dam = 3: slop = .6: ammo = 40
IF sw = 4 THEN IF sops < 2 THEN RETURN ELSE SOUND 200, .2: SOUND 75, .3: SOUND 100, .4: SOUND 50, .7: dam = 5: slop = 2: ammo = 30
IF sw = 5 THEN IF sops < .1 THEN RETURN ELSE SOUND 100, .1: SOUND 75, .2: SOUND 50, .2: dam = 2: slop = .1: ammo = 100
IF sw <> 1 THEN ammo(sw) = ammo(sw) - 1
GOSUB ammo
LOCATE 1, 2: PRINT "Reloading"
tim = TIMER
b = xb - gx: lb = ya - (80 + gm)
IF b < 1 OR lb < 1 OR b > 57 THEN RETURN
b = b \ 3 + 1: lb = lb \ 3: good = 0
IF gnome = 6 THEN IF fr > 3 THEN fr = fr - 1: good = 1
IF gnome = 2 AND fr > 4 THEN fr = fr - 1: good = 1
IF gnome = 3 AND fr > 4 AND ya > 124 THEN good = fr - 1: fr = 1
IF gnome = 3 AND fr > 4 THEN fr = fr - 2: good = 2: IF fr > 6 THEN fr = fr - 3: good = 5
IF fr > 0 THEN IF gnome <> 6 AND gnome <> 3 THEN b = b + 17 * fr ELSE b = b + 23 * fr
fr = fr + good
IF b > sef THEN b = b - sef: lb = lb + pool
IF MID$(vh$(lb), b + 1, 1) = "b" THEN glife = glife - dam: SOUND 100, .5: ggg = 10
IF MID$(vh$(lb), b + 1, 1) = "a" THEN glife = glife - dam * 2: SOUND 50, .5: ggg = 10
RETURN

gnomes:
IF fr < 0 THEN 26
GOSUB frame
IF gnome = 3 THEN IF fr = 9 OR fr = 6 THEN IF na = 1 THEN PUT (gx, 122), a(2 + (2860 * 6) + (2056 * (12 - 6)))
SELECT CASE gnome
CASE IS = 1
IF fr = 5 AND na = 0 THEN fr = 6: GOTO 9
CASE IS = 3
IF fr = 7 AND na = 0 THEN fr = 8: GOTO 9'3g
IF fr = 6 AND na = 0 THEN fr = 7: GOTO 9'3g
IF fr = 10 AND na = 0 THEN fr = 11: GOTO 9'3g
IF fr = 9 AND na = 0 THEN fr = 10: GOTO 9'3g
CASE IS = 2
IF fr = 8 AND na = 0 THEN fr = 9: GOTO 9'gw
IF fr = 7 AND na = 0 THEN fr = 8: GOTO 9'gw
IF fr = 6 AND na = 0 THEN fr = 7: GOTO 9'gw
IF fr = 5 AND na = 0 THEN fr = 6: GOTO 9
CASE IS = 4
IF fr = 6 AND na = 0 THEN fr = 7: GOTO 9
IF fr = 5 AND na = 0 THEN fr = 6: GOTO 9
CASE IS = 5
IF fr = 5 AND na = 0 THEN fr = 6: GOTO 9
CASE IS = 6
IF fr = 5 AND na = 0 THEN fr = 6: GOTO 9
IF fr = 4 AND na = 0 THEN fr = 5: GOTO 9
END SELECT
SELECT CASE gnome
CASE IS = 1
IF fr = 6 THEN fr = 5: na = 1: GOTO 9
CASE IS = 3
IF fr = 7 THEN fr = 6: na = 1: GOTO 9
IF fr = 8 THEN fr = 7: na = 1: GOTO 9
IF fr = 10 THEN fr = 9: na = 1: GOTO 9
IF fr = 11 THEN fr = 10: na = 1: GOTO 9
CASE IS = 2
IF fr = 6 THEN fr = 5: GOTO 9
IF fr = 9 THEN fr = 6: na = 1: GOTO 9
CASE IS = 4
IF fr = 6 THEN fr = 5: na = 1: GOTO 9
IF fr = 7 THEN fr = 6: na = 1: GOTO 9
CASE IS = 5
IF fr = 6 THEN fr = 5: na = 1: GOTO 9
CASE IS = 6
IF fr = 6 THEN fr = 0: na = 1: GOTO 9
IF fr = 5 THEN fr = 4: na = 1: GOTO 9
END SELECT
26 IF ggg = 11 THEN IF gnome <> 6 THEN fr = 4: GOTO 9 ELSE fr = 3: GOTO 9
IF fr = 2 OR fr = 3 THEN IF RND < .5 THEN IF gnome = 6 THEN gm = gm + 5: GOTO 9 ELSE GOTO 9
IF fr = 3 AND gnome = 5 THEN gx = gx - 5
IF fr > 0 THEN fr = 0: GOTO 9
14 fr = INT(RND * 6): na = 0
IF gnome = 6 THEN IF fr = 2 OR fr = 3 THEN gm = gm + 5
IF gnome = 6 AND RND < .6 AND fr <> 5 THEN fr = 1
IF fr = 3 AND gnome = 5 THEN gx = gx + 5
IF gnome = 3 AND fr = 5 THEN fr = 6
IF gnome = 3 AND RND < .12 THEN fr = 9
IF gnome = 3 THEN IF fr = 9 OR fr = 6 THEN PUT (gx, 122), a(2 + (2860 * 6) + (2056 * (12 - 6)))
IF fr = 1 THEN GOSUB walk
IF fr = 4 OR gnome = 3 AND fr = 5 THEN 14
IF gnome = 6 THEN IF fr > 2 THEN fr = fr - 1
9 IF qu = 1 THEN qu = 2: RETURN
IF gm > -5 AND gnome = 6 THEN gm = -5
GOSUB frame
RETURN

walk:
IF gx > 110 THEN wd = 1: GOTO 88
IF gx < 50 THEN wd = 2: GOTO 88
wd = INT(RND * 2) + 1
IF gnome = 6 THEN wd = wd + 3
88 IF wd = 1 THEN gx = gx - 10
IF wd = 2 THEN gx = gx + 10
IF wd = 4 THEN gx = gx - INT(RND * 5) - 1: gm = gm - INT(RND * 8) + INT(RND * 6)
IF wd = 5 THEN gx = gx + INT(RND * 5) + 1: gm = gm - INT(RND * 8) + INT(RND * 6)
IF gm > -5 AND gnome = 6 THEN fr = 2: gm = -5
IF gm < -40 THEN gm = -40
RETURN

yourhit:
IF gnome = 3 AND fr = 11 OR gnome <> 3 THEN IF blockb >= bb + gx AND blockb <= cb + gx AND blocka >= ba + 80 + gm AND blocka <= ca + 80 + gm THEN df = 0 ELSE df = 1
IF gnome = 3 AND fr = 8 THEN IF blockb >= bb2 + gx AND blockb <= cb2 + gx AND blocka >= ba2 + 80 + gm AND blocka <= ca2 + 80 + gm THEN df = 0 ELSE df = 1
IF df = 1 THEN GOTO hit ELSE GOTO block
hit: IF gnome = 1 THEN life = life - 1 ELSE IF gnome = 6 THEN life = life - 3
IF gnome = 2 THEN life = life - 2 ELSE IF gnome = 3 THEN life = life - 3
IF gnome = 4 THEN life = life - 1 ELSE IF gnome = 5 THEN life = life - 2
FOR i = 150 TO 50 STEP -15
SOUND i, .001 * i: SOUND 100, .1
NEXT i
GOSUB bar
RETURN

block: FOR i = 750 TO 250 STEP -150
SOUND i, .001 * (i / 3): SOUND 400 + i, .1
NEXT i
RETURN

traps4:
ty = 35: tx = 30 + INT(RND * 120)
trap = trap + 1: IF trap = 4 THEN GOTO 555
move3:
GET (tx - 4, ty - 4)-(tx + 4, ty + 4), ladder
GET (xb, ya)-(xb + 2, ya + 2), cur
LINE (xb, ya)-(xb + 2, ya + 2), 85, BF
fb = xb: fa = ya
a$ = INKEY$
GOSUB mousedrived
IF RIGHT$(a$, 1) = "H" THEN a$ = "8"
DRAW "bm=" + VARPTR$(tx) + ",=" + VARPTR$(ty)
DRAW "c17a0 e1r1f1d1g1l1h1u1br2bd1c14l1u1c12r1"
SOUND RND * 20 + 50, .7
PUT (fb, fa), cur, PSET
PUT (tx - 4, ty - 4), ladder, PSET
IF ty > 155 THEN xb = 30: ya = 30: GOSUB explode: GOTO 555
IF a$ = "8" THEN IF tx - 6 < xb AND xb < tx + 6 AND ty - 6 < ya AND ya < ty + 6 THEN GOSUB explode: GOTO traps4
ty = ty + 3
GOTO move3

traps3:
a = INT(RND * 4): SOUND 1000, .2
DRAW "a=" + VARPTR$(a)
IF a = 2 THEN ty = 155: tx = INT(RND * 100) + 30
IF a = 1 THEN tx = 20: ty = INT(RND * 80) + 30
IF a = 0 THEN ty = 35: tx = INT(RND * 100) + 30
IF a = 3 THEN tx = 200: ty = INT(RND * 80) + 30
GET (tx - 16, ty - 16)-(tx + 16, ty + 16), ladder
DRAW "bm=" + VARPTR$(tx) + ",=" + VARPTR$(ty)
DRAW "c10bl8bu2r16bd1c8bl3l10c7d1r10l5c5d1"
tim = TIMER + .3
move2:
GET (xb, ya)-(xb + 2, ya + 2), cur
LINE (xb, ya)-(xb + 2, ya + 2), 85, BF
fb = xb: fa = ya
IF TIMER - tim > .6 THEN CIRCLE (tx, ty), 2, 65, , , 3: CIRCLE (tx, ty), 1, 60: SOUND 100, .3: SOUND 50, .5: tim = TIMER: life = life - 2: GOSUB bar
a$ = INKEY$
GOSUB mousedrived
IF RIGHT$(a$, 1) = "H" THEN a$ = "8"
IF TIMER - tim > .1 THEN CIRCLE (tx, ty), 2, 8, , , 3: CIRCLE (tx, ty), 1, 8
PUT (fb, fa), cur, PSET
IF a$ = "8" THEN IF tx - 6 < xb AND xb < tx + 6 AND ty - 6 < ya AND ya < ty + 6 THEN PUT (tx - 16, ty - 16), ladder, PSET: GOTO explode
GOTO move2: GOTO 555

traps2:
tim = TIMER
tx = 80: ty = 80: ta = 0
move:
GET (xb, ya)-(xb + 2, ya + 2), cur
LINE (xb, ya)-(xb + 2, ya + 2), 85, BF
fb = xb: fa = ya
a$ = INKEY$
GOSUB mousedrived
GET (tx - 8, ty - 8)-(tx + 8, ty + 8), ladder
DRAW "ta=" + VARPTR$(ta)
DRAW "bm=" + VARPTR$(tx) + ",=" + VARPTR$(ty) + "c12r4d1c10nl7d1nl4c54r2c56h1l2u1c57r4"
SOUND RND * 20 + 50, .6
PUT (tx - 8, ty - 8), ladder, PSET
PUT (fb, fa), cur, PSET
IF TIMER - tim > 5 THEN GOTO explode
IF ta = 0 THEN tx = tx - 2: ty = ty - 0
IF ta = 180 THEN tx = tx + 2: ty = ty - 0
IF ta = 90 THEN tx = tx - 0: ty = ty + 2
IF ta = 270 THEN tx = tx - 0: ty = ty - 2
IF ta = 45 THEN tx = tx - 1.5: ty = ty + 1.5
IF ta = 135 THEN tx = tx + 1.5: ty = ty + 1.5
IF ta = 225 THEN tx = tx + 1.5: ty = ty - 1.5
IF ta = 315 THEN tx = tx - 1.5: ty = ty - 1.5
IF RND > .08 AND tx > 17 AND ty > 35 AND tx < 175 AND ty < 165 THEN GOTO move
IF RND < .5 OR tx < 17 OR ty < 35 OR tx > 175 OR ty > 165 THEN ta = ta - 45 ELSE ta = ta + 45
IF ta > 350 THEN ta = ta - 360
IF ta < 0 THEN ta = ta + 360
GOTO move

explode:
place(x, y) = -1
GET (tx - 16, ty - 16)-(tx + 16, ty + 16), ladder
FOR i = 1 TO 16
SOUND 50 + RND * 100, .3
CIRCLE (tx, ty), i, 55 + i / 2
SOUND 32676, .1
IF tx - 12 < xb AND tx + 12 > xb AND ty + 12 > ya AND ty - 12 < ya THEN IF i = 5 THEN PUT (tx - 16, ty - 16), ladder, PSET: IF trap < 4 THEN RETURN ELSE GOTO 555
NEXT i
SOUND 32676, 1
PUT (tx - 16, ty - 16), ladder, PSET
FOR i = 50 TO 100 STEP 20
SOUND i, .1
NEXT i
life = life - 5
GOSUB bar
GOTO 555

ammo:
alm = (ammo(sw) / ammo) * 300
LINE (310, 190)-(10 + alm, 191), 17, BF
LINE (10, 190)-(10 + alm, 190), 86
LINE (10, 191)-(10 + alm, 191), 89
RETURN

bar:
IF win = 1 THEN RETURN
PALETTE 0, 50
LINE (310, 184)-(10 + life * 6, 185), 17, BF
LINE (10, 184)-(10 + life * 6, 184), 106
LINE (10, 185)-(10 + life * 6, 185), 109
SOUND 32676, .1
PALETTE 0, 0
IF life < 0 THEN death: IF win = -1 OR win = -2 THEN 555
RETURN

mousedrived:
inary(ax) = 3
CALL INT86OLD(BOUSE, inary(), outary())
xb = outary(cx) / 2
ya = outary(dx)
bbb = outary(bx)
IF xb > 315 THEN xb = 315
IF ya > 195 THEN ya = 195
buttons = 0
IF bbb = 1 AND blt = 1 THEN buttons = 1: blt = 0: a$ = "H"
IF bbb = 2 AND blt = 1 THEN buttons = 2: blt = 0: a$ = "P"
IF bbb = 0 THEN blt = 1
RETURN

555 END SUB

SUB initialize
pig = 32000
DIM a(1 TO pig)
'-----------------------Finds wall to load-----------------
b$ = CHR$(p + 97)
q$ = "hall2" + b$: q1$ = "hall3" + b$: q2$ = "hall1" + b$
'------------------Creates and edits Palettes--------------------------
FOR i = 2 TO 256: lp(i) = 0: NEXT i
lp(1) = 50 + 256 * 50 + 65536 * 50
SCREEN 13
LOCATE 25, 1: COLOR 1: PRINT "LOADING..";
OPEN pal$ FOR INPUT AS #1
'change to "gnome2.pal"
FOR i = 1 TO 255 STEP 1
INPUT #1, pl(i)
NEXT i
PALETTE USING lp(0)
CLOSE
'----------------Loads pictures and gets walls--------------------------
DEF SEG = VARSEG(a(1))

BLOAD q$ + ".pic", VARPTR(a(1))
PUT (1, 1), a, PSET
GET (1, 2)-(43, 149), c
GET (2 + 165, 2)-(43 + 166, 149), d
GET (44, 30)-(68, 120), f
GET (44 + 97, 30)-(67 + 99, 120), g
GET (69, 48)-(84, 103), h
GET (69 + 56, 48)-(85 + 56, 103), i
GET (85, 48)-(69 + 56, 103), j
DEF SEG = VARSEG(a(1))
IF qq = 8 OR qq = 10 OR qq = 11 THEN GOTO 34
BLOAD "well.pic", VARPTR(a(1))
PUT (1, 1), a, PSET
GET (1, 1)-(124, 91), bw
GET (125, 1)-(197, 55), ew
GET (125, 56)-(166, 86), hw
GET (1, 92)-(101, 116), bbb
GOTO 33
34 DEF SEG = VARSEG(a(1))
BLOAD "hostages.pic", VARPTR(a(1))
PUT (1, 1), a, PSET: hos = 1
IF qq = 8 THEN GET (1, 1)-(33, 92), bw
IF qq = 10 THEN GET (34, 1)-(72, 92), bw
IF qq = 11 THEN GET (73, 1)-(138, 92), bw
GET (1, 93)-(101, 117), bbb
33 DEF SEG = VARSEG(a(1))
BLOAD q1$ + ".pic", VARPTR(a(1))
PUT (1, 1), a, PSET
GET (1, 1)-(124, 91), b
GET (125, 1)-(197, 55), e
GET (126, 56)-(144, 74), bl
GET (1, 92)-(42, 121), ch
GET (43, 92)-(64, 105), chh
GET (65, 92)-(88, 124), bwr
GET (89, 92)-(121, 121), awr
GET (122, 92)-(145, 124), cwr
GET (148, 92)-(176, 118), ch1
GET (177, 92)-(205, 118), ch2
DEF SEG = VARSEG(a(1))
BLOAD q2$ + ".pic", VARPTR(a(1))
PUT (1, 1), a, PSET
GET (105, 2)-(147, 149), di
GET (63, 2)-(105, 149), ci
GET (105, 30)-(105 + 24, 120), Gi
GET (105 - 24, 30)-(105, 120), fi
GET (105, 48)-(105 + 16, 103), ii
GET (105 - 16, 48)-(105, 103), hi
GET (1, 2)-(43, 149), ck
GET (2 + 165, 2)-(43 + 166, 149), dk
GET (44, 30)-(68, 120), fk
GET (44 + 97, 30)-(67 + 99, 120), gk
GET (69, 48)-(84, 103), hk
GET (69 + 56, 48)-(85 + 56, 103), ik
GET (85, 48)-(69 + 56, 103), jk
GET (68, 30)-(44 + 97, 47), ak
GET (68, 101)-(44 + 97, 126), al
IF hos <> 1 THEN PUT (69 + 16, 48 + 12), hw, PSET
IF hos <> 1 THEN GET (85, 48)-(69 + 56, 103), hw
CLS
PUT (1 + 5, 1 + 20), a, PSET
ERASE a
'-----------------------Draws box around 3D view-------------------------
FOR f = 1 TO 5
LINE (f, 16 + f)-(220 - f, 175 - f), 44 - f, B
NEXT f
FOR f = 1 TO 3
LINE (f, -1)-(320 - f, 20 - f), 20 - f, B
NEXT f
FOR f = 1 TO 8
IF f <> 8 THEN LINE (f, f + 174)-(320 - f, 200 - f), 25 - f, B ELSE LINE (f, f + 174)-(320 - f, 200 - f), 25 - f, BF
NEXT f
PALETTE USING pl(0)
COLOR 54
PUT (219, 150), bbb, PSET
END SUB

SUB view3d
'-------------CALCULATES SCENE-------------
uf = 0
DIM plod(-1 TO 1, 0 TO 3): dor = 0
IF d = 1 THEN S = 0: c1 = -2: c = 1: e = -1
IF d = 2 THEN S = 1: c1 = 2: c = -1: e = -1
IF d = 3 THEN S = 0: c1 = 2: c = -1: e = 1
IF d = 4 THEN S = 1: c1 = -2: c = 1: e = 1
a = c1: b = 0
r = 0: q = -2
3 a = a + c: q = q + 1
IF S = 1 THEN SWAP a, b
pla = place(x + a, y + b)
IF pla > 0 AND pla <> 5 AND pla <> 9 AND pla <> 6 AND pla <> 12 AND pla <> 13 AND pla <> 14 AND pla <> 15 AND pla <> 16 AND pla <> 17 AND pla <> 20 AND pla <> 21 AND (pla <> 19 OR qq <> 20) THEN plod(q, r) = 1
qr = r * 10 + q
IF pla < 2 THEN 19
SELECT CASE qr
CASE IS = 10
IF pla = 2 THEN dor = 1: uf = 3
IF pla = 8 THEN sp = 1
IF pla = 3 THEN ch = 1: uf = 4
IF pla = 4 THEN rh = 1: uf = 5
IF pla = 6 THEN bh = 1
IF pla = 10 THEN ow = 1
IF pla = 11 THEN ky = 1: uf = 2
IF pla = 12 THEN g = 2
IF pla = 7 THEN wh = 1: uf = 1
IF pla = 18 THEN bb = 1: uf = 6
IF pla = 5 OR qq > 16 AND pla = 16 THEN ph = 1
IF pla = 16 AND qq < 17 THEN lh = 1
IF pla = 19 AND qq <> 20 THEN hos = 1: uf = 7

CASE IS = 20
IF pla = 3 THEN ch = 2
IF pla = 5 OR qq > 16 AND pla = 16 THEN IF plod(0, 1) <> 1 THEN ph = 2
IF pla = 6 THEN IF plod(0, 1) <> 1 THEN bh = 2
IF pla = 16 AND qq < 17 THEN IF plod(0, 1) <> 1 THEN lh = 2
IF pla = 7 THEN wh = 2

CASE IS = 30
IF pla = 5 OR qq > 16 AND pla = 16 THEN IF plod(0, 2) <> 1 AND plod(0, 1) <> 1 THEN ph = 3
IF pla = 6 THEN IF plod(0, 2) <> 1 AND plod(0, 1) <> 1 THEN bh = 3
IF pla = 16 AND qq < 17 THEN IF plod(0, 2) <> 1 AND plod(0, 1) <> 1 THEN lh = 3
IF pla = 7 THEN wh = 3

CASE IS = 0
IF pla = 9 THEN iw = 1

CASE ELSE
IF q = -1 AND r = 0 THEN IF pla = 4 THEN rh = 2
IF q = 1 AND r = 0 THEN IF pla = 4 THEN rh = 3
IF q = -1 AND r = 0 THEN IF pla = 18 THEN bb = 2
IF q = 1 AND r = 0 THEN IF pla = 18 THEN bb = 3
IF q = -1 AND r = 0 THEN IF pla = 11 THEN ky = 2
IF q = 1 AND r = 0 THEN IF pla = 11 THEN ky = 3
IF q = -1 AND r = 0 THEN IF pla = 3 THEN chh = 1
IF q = 1 AND r = 0 THEN IF pla = 3 THEN chh = 2

END SELECT

19 IF S = 1 THEN SWAP a, b
IF q = 1 THEN 4
GOTO 3
4 r = r + 1: b = b + e
IF r > 3 THEN 5 ELSE q = -2: a = c1
GOTO 3
'---------------DRAWS SCENE-----------------------
5 IF iw = 1 AND ow = 1 THEN plod(0, 1) = 0
IF plod(-1, 1) <> 1 AND plod(-1, 0) <> 1 THEN PUT (1 + 5, 2 + 20), ci, PSET: x1 = 1
IF plod(1, 1) <> 1 AND plod(1, 0) <> 1 THEN PUT (2 + 5 + 165, 2 + 20), di, PSET: y1 = 1
IF x1 <> 1 THEN IF plod(-1, 0) <> 1 AND plod(-1, 1) = 1 THEN PUT (1 + 5, 2 + 20), c, PSET ELSE PUT (1 + 5, 2 + 20), ck, PSET: GOSUB 52
IF y1 <> 1 THEN IF plod(1, 0) <> 1 AND plod(1, 1) = 1 THEN PUT (2 + 5 + 165, 2 + 20), d, PSET ELSE PUT (2 + 5 + 165, 2 + 20), dk, PSET: GOSUB 53
IF lch = 1 THEN GOSUB 93
IF plod(0, 1) = 1 AND wh <> 1 THEN PUT (44 + 5, 30 + 20), b, PSET: g = 1: GOSUB 1: IF dor = 1 THEN PUT (44 + 5, 67 + 20), bl, PSET: GOTO done ELSE GOTO done
IF plod(0, 1) = 1 AND wh = 1 THEN PUT (44 + 5, 30 + 20), bw, PSET: g = 1: GOSUB 1: IF dor = 1 THEN PUT (44 + 5, 67 + 20), bl, PSET: GOTO done ELSE GOTO done
GOSUB 1
PUT (68 + 5, 30 + 20), ak, PSET
PUT (68 + 5, 101 + 20), al, PSET

IF plod(-1, 1) <> 1 AND plod(-1, 2) <> 1 THEN PUT (44 + 5, 30 + 20), fi, PSET: x2 = 1
IF plod(1, 1) <> 1 AND plod(1, 2) <> 1 THEN PUT (44 + 5 + 97, 30 + 20), Gi, PSET: y2 = 1

IF x2 <> 1 THEN IF plod(-1, 1) <> 1 AND plod(-1, 2) = 1 THEN PUT (44 + 5, 30 + 20), f, PSET ELSE PUT (44 + 5, 30 + 20), fk, PSET
IF y2 <> 1 THEN IF plod(1, 1) <> 1 AND plod(1, 2) = 1 THEN PUT (44 + 5 + 97, 30 + 20), g, PSET ELSE PUT (44 + 5 + 97, 30 + 20), gk, PSET
IF plod(0, 2) = 1 AND wh <> 2 THEN PUT (69 + 5, 48 + 20), e, PSET: GOSUB 51: GOTO done
IF plod(0, 2) = 1 AND wh = 2 THEN PUT (69 + 5, 48 + 20), ew, PSET: GOSUB 51: GOTO done

IF plod(-1, 2) <> 1 AND plod(-1, 3) <> 1 THEN PUT (69 + 5, 48 + 20), hi, PSET: x3 = 1
IF plod(1, 2) <> 1 AND plod(1, 3) <> 1 THEN PUT (69 + 5 + 56, 48 + 20), ii, PSET: y3 = 1

IF x3 <> 1 THEN IF plod(-1, 2) <> 1 AND plod(-1, 3) = 1 THEN PUT (69 + 5, 48 + 20), h, PSET ELSE PUT (69 + 5, 48 + 20), hk, PSET
IF y3 <> 1 THEN IF plod(1, 2) <> 1 AND plod(1, 3) = 1 THEN PUT (69 + 5 + 56, 48 + 20), i, PSET ELSE PUT (69 + 5 + 56, 48 + 20), ik, PSET

IF wh <> 3 THEN IF plod(0, 3) = 1 THEN PUT (85 + 5, 48 + 20), j, PSET ELSE PUT (85 + 5, 48 + 20), jk, PSET
IF wh = 3 THEN IF plod(0, 3) = 1 THEN PUT (85 + 5, 48 + 20), hw, PSET ELSE PUT (85 + 5, 48 + 20), jk, PSET
GOTO done:

93 IF p = 0 THEN LINE (115, 141)-(156, 143), 111, BF: lch = 0
IF p = 1 THEN LINE (115, 141)-(156, 143), 20, BF: lch = 0
IF p = 3 THEN LINE (115, 141)-(156, 143), 18, BF: lch = 0
IF p = 2 THEN LINE (115, 141)-(156, 144), 156, BF: lch = 0
IF p = 4 THEN LINE (115, 141)-(156, 144), 88, BF: lch = 0
RETURN
1 IF ch = 1 THEN PUT (110 + 5, 100 + 15), ch, PSET: lch = 1
IF rh = 1 THEN PUT (92, 77), awr, PSET
IF ky <> 0 THEN GOSUB keey
IF bb = 1 THEN GOSUB button
IF hos = 1 THEN GOSUB hostage
RETURN
hostage:
GET (85, 141)-(150, 169), ladder
PUT (85, 78), bw, PSET
had = 1
LOCATE 1, 2: PRINT "A hostage! (Hit a key to continue)"
WHILE INKEY$ = "": WEND

RETURN
51 IF ch = 2 THEN PUT (110 + 5, 100 + 12), chh, PSET
RETURN
52 IF rh = 2 THEN PUT (6, 74), bwr, PSET
IF bb = 2 THEN GOSUB button3
RETURN
53 IF rh = 3 THEN PUT (190, 74), cwr, PSET
IF bb = 3 THEN GOSUB button2
RETURN
bars:
g = 1
FOR t = 60 TO 160 STEP 10
LINE (t, 55)-(t, 135), 19 + m
LINE (t + 1, 55)-(t + 1, 135), 21 + m
NEXT t
FOR t = 75 TO 115 STEP 20
LINE (55, t)-(165, t), 19 + m
LINE (55, t + 1)-(165, t + 1), 21 + m
NEXT t
RETURN
bars2:
FOR t = 85 TO 135 STEP 5.5
LINE (t, 75)-(t, 118), 22 + m
NEXT t
FOR t = 85 TO 108 STEP 11
LINE (85, t)-(133, t), 23 + m
NEXT t
RETURN
bars3:
FOR t = 95 TO 124 STEP 3.5
LINE (t, 82)-(t, 110), 24 + m
NEXT t
FOR t = 88 TO 103 STEP 7
LINE (95, t)-(123, t), 25 + m
NEXT t
RETURN
ladder3:
GET (79, 25)-(139, 35), ladder
CIRCLE (109, 30), 27, 20, , , .15
PAINT (109, 30), 1, 20: lad = 1
RETURN
ladder2:
CIRCLE (109, 57), 15, 23, , , .15
PAINT (109, 57), 1, 23
RETURN
ladder:
CIRCLE (109, 74), 9, 24, , , .15
PAINT (109, 74), 1, 24
RETURN
keey:
IF p = 1 OR 5 THEN l = 43
IF p = 2 THEN l = 1
IF p = 3 THEN l = 140
IF p = 0 THEN l = -8
l1 = l + 17
l2 = l + 12
l3 = l + 14
l4 = l + 15
IF ky = 1 THEN CIRCLE (150, 78), 3, l1, , , 1.8
IF ky = 2 THEN CIRCLE (38, 78), 3, l1, , , 1.8
IF ky = 3 THEN CIRCLE (209, 78), 3, l1, , , 1.8
IF ky = 1 THEN DRAW "a3bm149,80 c=" + VARPTR$(l2) + " nr2u1r1u1r1c=" + VARPTR$(l3) + "f1nl2r8 c=" + VARPTR$(l4) + "d1r2u1"
IF ky = 2 THEN DRAW "a3bm38,80 c=" + VARPTR$(l2) + " nr2u1r1u1r1c=" + VARPTR$(l3) + "f1nl2r9 c=" + VARPTR$(l4) + "d1r2u1"
IF ky = 3 THEN DRAW "a3bm209,80 c=" + VARPTR$(l2) + " nr2u1r1u1r1c=" + VARPTR$(l3) + "f1nl2r8 c=" + VARPTR$(l4) + "d1r2u1"
RETURN

button:
CIRCLE (151, 80), 4, 58: CIRCLE (150, 80), 4, 56
PAINT (150, 80), 56: CIRCLE (150, 80), 3, 54
RETURN
button2:
CIRCLE (209, 80), 5, 58, , , 3: CIRCLE (208, 80), 5, 56, , , 3
PAINT (208, 80), 56: CIRCLE (208, 80), 4, 54, , , 3
RETURN
button3:
CIRCLE (39, 80), 4, 58, , , 3: CIRCLE (38, 80), 4, 56, , , 3
PAINT (38, 80), 56: CIRCLE (38, 80), 3, 54, , , 3
RETURN

done:
IF chh = 2 THEN PUT (167 + 5, 119 + 12), ch1, PSET
IF chh = 1 THEN PUT (15 + 5, 119 + 12), ch2, PSET
IF sp = 1 THEN g = 0
IF p = 0 THEN m = 85
IF p = 1 THEN m = 0
IF p = 2 THEN m = -2
IF p = 3 THEN m = 135
IF p = 4 THEN m = -13
IF bh = 1 THEN GOSUB bars
IF bh = 2 THEN GOSUB bars2
IF bh = 3 THEN GOSUB bars3
IF lh = 1 THEN IF p <> 0 THEN GOSUB ladder3: lad = 1 ELSE ph = 1
IF lh = 2 THEN IF p <> 0 THEN GOSUB ladder2 ELSE ph = 2
IF lh = 3 THEN IF p <> 0 THEN GOSUB ladder ELSE ph = 3
IF lh <> 1 AND lad = 1 THEN PUT (79, 25), ladder, PSET: lad = 0
IF hos <> 1 AND had = 1 THEN PUT (85, 141), ladder, PSET: had = 0
END SUB

DEFSNG P
SUB wins
FOR i = 1 TO 20: a$ = INKEY$: NEXT i
DIM a(1 TO 32000)
LOCATE 1, 2: PRINT "Hit space to continue"
DEF SEG = VARSEG(a(1))
BLOAD "fin2.pic", VARPTR(a(1))
PUT (1 + 5, 1 + 20), a, PSET
SOUND 32676, 10: SOUND 100, .1
WHILE INKEY$ <> " ": WEND

CLS
SCREEN 12
OPEN "subplot.dat" FOR INPUT AS #1
INPUT #1, l
FOR i = 1 TO 8
INPUT #1, f$
PRINT f$
NEXT i
CLOSE
WHILE INKEY$ = "": WEND
SCREEN 13

OPEN "gnome.pal" FOR INPUT AS #1
FOR i = 1 TO 255 STEP 1
INPUT #1, pl(i)
NEXT i: PALETTE USING pl(1): CLOSE

DEF SEG = VARSEG(a(1))
BLOAD "hall1a.pic", VARPTR(a(1))
PUT (1 + 5, 1 + 20), a, PSET

tim = TIMER
VIEW SCREEN (6, 21)-(215, 170)
41 FOR t = 40 TO 1 STEP -1
CIRCLE (110, 95), t, 78 + t / 3 + RND * 8, , , 1.5
NEXT t
IF TIMER - tim < 3 THEN GOTO 41
FOR t = 1 TO 100
CIRCLE (110, 95), t, 54 + t / 5 + RND * 3, , , .9
SOUND 50 + RND * 200, .1
NEXT t
tim = TIMER

VIEW
CLS
win = 1: sw = 1
gnome2 = 4: gnome3 = 4
LOCATE 2, 1: PRINT "Bad concept: ANDRE GRATTO"
LOCATE 3, 1: PRINT "Bad design: JON KREUZER and ANDRE GRATTO"
fight
CLS
gnome2 = 1: gnome3 = 1
LOCATE 2, 1: PRINT "    Programming: JON KREUZER"
LOCATE 3, 1: PRINT "Bad programming: Andre Gratto"
fight
CLS
gnome2 = 2: gnome3 = 2
LOCATE 2, 1: PRINT "Art Director   : JON KREUZER"
LOCATE 3, 1: PRINT "Art            : JOSH HESS"
LOCATE 4, 1: PRINT "                 Jon Kreuzer"
fight
CLS
gnome2 = 5: gnome3 = 5
LOCATE 2, 1: PRINT "Bad Sound      : JON KREUZER"
LOCATE 3, 1: PRINT "                            "
fight
CLS
gnome2 = 3: gnome3 = 3
LOCATE 2, 1: PRINT "Produced by: Kreuzer Industries"
LOCATE 3, 1: PRINT "                               "
fight
CLS
gnome2 = 6: gnome3 = 6
LOCATE 2, 1: PRINT "This game took 4 months and costed"
LOCATE 3, 1: PRINT "$10.50 to create. We may never be "
LOCATE 4, 1: PRINT "reimbursed."
fight
SCREEN 0: WIDTH 80: PRINT "Thank you for playing"
SYSTEM
END SUB

