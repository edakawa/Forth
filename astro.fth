\
\ Author: Hajime EDAKAWA <hajime.edakawa@gmail.com>
\ License: Public Domain
\ Last Update: Nov 2019
\
\ Reference:
\   Kou NAGASAWA, Computations of Sunrise and Sunset, Chijin-Shokan, Japan.
\

\ ------------------------------------------------------------------------------

3.14159265358979e  FCONSTANT $PI          \ 円周率

360e        24e F/ FCONSTANT $HMS-H-UNIT  \ HMS 表記の単位:Hour
$HMS-H-UNIT 60e F/ FCONSTANT $HMS-M-UNIT  \ HMS 表記の単位:Minute
$HMS-M-UNIT 60e F/ FCONSTANT $HMS-S-UNIT  \ HMS 表記の単位:Second

             1e    FCONSTANT $DMS-D-UNIT  \ DMS 表記の単位:Degree
$DMS-D-UNIT 60e F* FCONSTANT $DMS-M-UNIT  \ DMS 表記の単位:Minute
$DMS-M-UNIT 60e F* FCONSTANT $DMS-S-UNIT  \ DMS 表記の単位:Second

\ ------------------------------------------------------------------------------

: F, ( r -- ) FALIGN HERE F! 1 FLOATS ALLOT ;
: FVALUE ( r "name" -- ) CREATE F, DOES> F@ ;
: [FTO]  ( r "name" -- ) ' >BODY POSTPONE LITERAL POSTPONE F! ; IMMEDIATE

\
\ The source of UNDER and WITHIN words is the book of "Thinking Forth".
\
\ Reference:
\   Leo Brodie (1984, 1994 and 2004) Thinking Forth,
\     Forth Interest Group: http://www.forth.org
\ 
: FUNDER* ( r1 r2 r3 -- r1*r3 r2 ) FROT F* FSWAP ;
: FUNDER/ ( r1 r2 r3 -- r1/r3 r2 ) FROT FSWAP F/ FSWAP ;
: FUNDER*+ ( r1 r2 r3 -- r1*r3+r2 ) FUNDER* F+ ;
: F*+ ( r1 r2 r3 -- r1+r2*r3 ) F* F+ ;

: FNIP ( r1 r2 -- r2 ) FSWAP FDROP ;
: 2FDUP ( r1 r2 -- r1 r2 r1 r2 ) FOVER FOVER ;
: FTUCK ( r1 r2 -- r2 r1 r2 ) FSWAP FOVER ;
: F<= ( r1 r2 -- r1<=r2 ) 2FDUP F< F- F0= OR ;
: FWITHIN  ( F: x lo hi -- S: lo<=x<hi ) FSWAP 2 FPICK F<= F< AND ;

: RAD>DEG [ 180e $PI F/ ] FLiteral F* ;
: DEG>RAD [ $PI 180e F/ ] FLiteral F* ;

: %FSIN DEG>RAD FSIN ;
: %FCOS DEG>RAD FCOS ;
: %FTAN DEG>RAD FTAN ;

: %FASIN FASIN RAD>DEG ;
: %FACOS FACOS RAD>DEG ;
: %FATAN FATAN RAD>DEG ;

:    SAVE-DS-SIGN  s" DUP 0< IF ABS -1 ELSE 1 THEN >R"    EVALUATE ; immediate
: RESTORE-DS-SIGN  s" R> S>F F*"                          EVALUATE ; immediate
:    SAVE-FS-SIGN  s" FDUP F0< IF FABS -1 ELSE 1 THEN >R" EVALUATE ; immediate
: RESTORE-FS-SIGN  s" R> *"                               EVALUATE ; immediate

: .YYYY/MM/DD ( year month day -- )
  ROT 10000 * ROT 100 * + + S>D
  <# # # 47 HOLD # # 47 HOLD # # # # #> TYPE ;

: .HH:MM ( hour minute -- )
  SWAP 100 * + S>D
  <# # # 58 HOLD # # #> TYPE ;

\ ------------------------------------------------------------------------------

: HMS>DEG  ( S: hour F: minute second -- F: degree )
  SAVE-DS-SIGN
  $HMS-S-UNIT F*  $HMS-M-UNIT FUNDER*  F+  S>F $HMS-H-UNIT F*  F+
  RESTORE-DS-SIGN ;

: DEG>HMS  ( F: degree -- S: hour F: minute second )
  SAVE-FS-SIGN
  FDUP                             ( F: d d     S:   )
  $HMS-H-UNIT F/ FLOOR  FDUP  F>S  ( F: d d     S: h )
  $HMS-H-UNIT F* F-                ( F: d'      S: h )
  FDUP                             ( F: d' d'   S: h )
  $HMS-M-UNIT F/ FLOOR  FTUCK      ( F: m d' m  S: h )
  $HMS-M-UNIT F* F-                ( F: m d''   S: h )
  $HMS-S-UNIT F/                   ( F: m s     S: h )
  RESTORE-FS-SIGN ;

: DMS>DEG  ( S: degree F: minute second -- F: degree )
  SAVE-DS-SIGN
  $DMS-S-UNIT F/  $DMS-M-UNIT FUNDER/  F+  S>F $DMS-D-UNIT F/  F+
  RESTORE-DS-SIGN ;

: DEG>DMS  ( F: degree -- S: degree F: minute second )
  SAVE-FS-SIGN
  FDUP                             ( F: r r     S:   )
  $DMS-D-UNIT F* FLOOR  FDUP  F>S  ( F: r d     S: d )
  $DMS-D-UNIT F/ F-                ( F: r'      S: d )
  FDUP                             ( F: r' r'   S: d )
  $DMS-M-UNIT F* FLOOR  FTUCK      ( F: m r' m  S: d )
  $DMS-M-UNIT F/ F-                ( F: m r''   S: d )
  $DMS-S-UNIT F*                   ( F: m s     S: d )
  RESTORE-FS-SIGN ;

\ ------------------------------------------------------------------------------

: TIME ( S: year month day F: hour -- F: time ) \ ユリウス時
  2 PICK ROT ROT 2 PICK 2 PICK  ( S: y y m d y m  F:                  )
  S>F 1e F+ 3e F* 5e F/ FLOOR   ( S: y y m d y    F: h m1             )
  S>F 4e F/ FLOOR               ( S: y y m d      F: h m1 y1          )
  S>F 33.875e F-                ( S: y y m        F: h m1 y1 d1       )
  S>F 30e F*                    ( S: y y          F: h m1 y1 d1 m2    )
  S>F 365e F*                   ( S: y            F: h m1 y2 d1 m2 y2 )
  F+ F+ F+ F+ FSWAP             ( S: y            F: K h              )
  24e F/                        ( S: y            F: K h1             )
  S>F 65e F+ 86400e F/          ( S:              F: K h1 d           )
  F+ F+ 365.25e F/ ;            ( S:              F: [K+h1+d]/365.25  )

\ ------------------------------------------------------------------------------

: EPSILON ( F: time -- F: obliquity_of_the_ecliptic ) \ 黄道傾角
  23.439291e 0.000130042e FROT F* F- ;

\ ------------------------------------------------------------------------------

: AU ( F: time -- F: r ) \ 天文単位
  FDUP 0.007256e 0.0000002e FUNDER* FSWAP F-          ( F: t A         )
  FSWAP                                               ( F: A t         )
  FDUP 267.54e 359.991e FUNDER*+ %FSIN FROT F*        ( F: t X=B*A     )
  FOVER 265.10e 719.98e FUNDER*+ %FSIN 0.000091e F*+  ( F: t X+Y1      )
  90.0e                          %FSIN 0.000030e F*+  ( F: t X+Y1+Y2.. )
  FOVER  27.8e 4452.67e FUNDER*+ %FSIN 0.000013e F*+
  FOVER 254.0e  450.40e FUNDER*+ %FSIN 0.000007e F*+
  FOVER 156.0e  329.60e FUNDER*+ %FSIN 0.000007e F*+
  FNIP                                                ( F: Z=X+Y1+Y2.. )
  10e FSWAP F** ;                                     ( F: 10^Z        )

\ ------------------------------------------------------------------------------

: LAMBDA.S  ( F: time -- F: ecliptic_longtitude )  \ 太陽の視黄道
  FDUP 280.4603e 360.00769e FUNDER*+                  ( F: t A          )
  FSWAP                                               ( F: A t          )
  FDUP 1.9146e 0.00005e FUNDER* FSWAP F-              ( F: A t B        )
  FSWAP                                               ( F: A B t        )
  FDUP 357.5380e 359.99100e FUNDER*+ %FSIN            ( F: A B t C      )
  FROT F* FROT F+                                     ( F: t X=C*B+A    )
  FOVER 355.05e  719.981e FUNDER*+ %FSIN 0.0200e F*+  ( F: t X+Y1       )
  FOVER 234.95e   19.341e FUNDER*+ %FSIN 0.0048e F*+  ( F: t X+Y1+Y2... )
  FOVER 247.10e  329.640e FUNDER*+ %FSIN 0.0020e F*+
  FOVER 297.80e 4452.670e FUNDER*+ %FSIN 0.0018e F*+
  FOVER 251.30e    0.200e FUNDER*+ %FSIN 0.0018e F*+
  FOVER 343.20e  450.370e FUNDER*+ %FSIN 0.0015e F*+
  FOVER  81.40e  225.180e FUNDER*+ %FSIN 0.0013e F*+
  FOVER 132.50e  659.290e FUNDER*+ %FSIN 0.0008e F*+
  FOVER 153.30e   90.380e FUNDER*+ %FSIN 0.0007e F*+
  FOVER 206.80e   30.350e FUNDER*+ %FSIN 0.0007e F*+
  FOVER  29.80e  337.180e FUNDER*+ %FSIN 0.0006e F*+
  FOVER 207.40e    1.500e FUNDER*+ %FSIN 0.0005e F*+
  FOVER 291.20e   22.810e FUNDER*+ %FSIN 0.0005e F*+
  FOVER 234.90e  315.560e FUNDER*+ %FSIN 0.0004e F*+
  FOVER 157.30e  299.300e FUNDER*+ %FSIN 0.0004e F*+
  FOVER  21.10e  720.020e FUNDER*+ %FSIN 0.0004e F*+
  FOVER 352.50e 1079.970e FUNDER*+ %FSIN 0.0003e F*+
  FOVER 329.70e   44.430e FUNDER*+ %FSIN 0.0003e F*+
  FNIP                                                ( F: Z=X+Y1+Y2...   )
  BEGIN FDUP            F0< WHILE 360e F+ REPEAT      ( F: Z<0   => Z+360 )
  BEGIN FDUP 360e FSWAP F<  WHILE 360e F- REPEAT ;    ( F: 360<Z => Z-360 )

\ ------------------------------------------------------------------------------

: ALPHA  ( epsilon lambda.s -- right_ascension )  \ 赤経
  FTUCK                                 ( F: l e l             )
  %FTAN FSWAP %FCOS F*                  ( F: l x=tan[l]*cos[e] )
  %FATAN                                ( F: l a=atan[x]       )
  FSWAP                                 ( F: a l               )
  0e 180e FWITHIN IF                    ( F: a                 )
    FDUP F0< IF 180e F+ THEN
  ELSE
    FDUP F0< IF 360e ELSE 180e THEN F+
  THEN ;

\ ------------------------------------------------------------------------------

: DELTA  ( F: epsilon lambda.s -- F: declination )  \ 赤緯
  %FSIN FSWAP %FSIN F* %FASIN ;  ( F: asin[sin[l]*sin[e]] )

\ ------------------------------------------------------------------------------

: THETA  ( F: longtitude day time -- F: sidereal_time )  \ 恒星時
  FDUP FDUP F* 0.00000003879e F*  ( F: l d t A*t^2    )
  360.007700536e FUNDER*          ( F: l d B*t A*t^2  )
  325.4606e F+ F+                 ( F: l d C          )
  24e FUNDER/ 360e FUNDER* F+ F+  ( F: Z=360d+C+l     )
  BEGIN FDUP            F0< WHILE 360e F+ REPEAT
  BEGIN FDUP 360e FSWAP F<  WHILE 360e F- REPEAT ;

\ ------------------------------------------------------------------------------

0e FVALUE $H  \ 観測者の高さ (単位:メートル)

: ALTITUDE  ( F: au -- F: altitude )  \ 太陽の出没高度
  FDUP 0.266994e FSWAP F/ FNEGATE ( F: au -S     )
  0.035333e $H FSQRT F* F-        ( F: au -S-E   )      
  0.585556e F-                    ( F: au -S-E-R )
  0.0024428e FROT F/ F+ ;         ( F: altitude  )

\ ------------------------------------------------------------------------------

0 VALUE #DBG    \ -1=ON, 0=OFF

0 VALUE #YEAR   \ 2000+Y
0 VALUE #MONTH  \ 月
0 VALUE #DAY    \ 日

0e FVALUE #TV          \ 時刻変数
0e FVALUE #LATITUDE    \ 緯度
0e FVALUE #LONGTITUDE  \ 経度
0e FVALUE #TIME        \ 経過ユリウス時
0e FVALUE #EPSILON     \ 黄道傾角
0e FVALUE #LAMBDA.S    \ 太陽の視黄道
0e FVALUE #ALPHA       \ 太陽の赤経
0e FVALUE #DELTA       \ 太陽の赤緯
0e FVALUE #AU          \ 太陽との距離
0e FVALUE #ALTITUDE    \ 太陽の出没高度
0e FVALUE #THETA       \ グリニジ時
0e FVALUE #DIFF        \ 差
0e FVALUE #TK          \ XXX
0e FVALUE #T           \ XXX

\ ------------------------------------------------------------------------------

: SET.LAT/LNG  ( F: latitude longtitude -- )
  [FTO] #LONGTITUDE [FTO] #LATITUDE ;

: UPDATE ( -- )
  #YEAR #MONTH #DAY #TV TIME     [FTO] #TIME
  #TIME                 EPSILON  [FTO] #EPSILON
  #TIME                 LAMBDA.S [FTO] #LAMBDA.S
  #EPSILON #LAMBDA.S    ALPHA    [FTO] #ALPHA
  #EPSILON #LAMBDA.S    DELTA    [FTO] #DELTA
  #TIME                 AU       [FTO] #AU
  #AU                   ALTITUDE [FTO] #ALTITUDE
  #LONGTITUDE #TV #TIME THETA    [FTO] #THETA

  #DBG IF
    CR ." --------------------------------------------------"
    CR ."   #TIME     " #TIME F.
    CR ."   #EPSILON  " #EPSILON F.
    CR ."   #LAMBDA.S " #LAMBDA.S F.
    CR ."   #ALPHA    " #ALPHA F.
    CR ."   #DELTA    " #DELTA F.
    CR ."   #AU       " #AU F.
    CR ."   #ALTITUDE " #ALTITUDE F.
    CR ."   #THETA    " #THETA F.
    CR ." --------------------------------------------------"
  THEN ;

: ITER  ( S: year month day -- ) \ 日の出時刻を計算する
  TO #DAY  TO #MONTH  TO #YEAR
  6e [FTO] #TV
  0e [FTO] #DIFF
  BEGIN
    UPDATE

    #ALTITUDE %FSIN                     ( F: sin[a]                 )
    #DELTA %FSIN #LATITUDE %FSIN F* F-  ( F: X=sin[a]-sin[d]*sin[l] )
    #DELTA %FCOS #LATITUDE %FCOS F*     ( F: X Y=cos[d]*cos[l]      )
    F/ %FACOS                           ( F: Z=acos[X/Y]            )
    FDUP F0< INVERT IF FNEGATE THEN     ( F: 0<Z => -Z              )
                                       [FTO] #TK
    #THETA #ALPHA F-                   [FTO] #T
    #TK #T F- 360e F/                  [FTO] #DIFF
    #DIFF #DIFF FLOOR F-               [FTO] #DIFF
    #DIFF 0.5e FSWAP F< IF #DIFF 1e F- [FTO] #DIFF THEN
    #TV #DIFF 24e F* F+                [FTO] #TV

    #DBG IF
      CR ." TK   = " #TK F.
      CR ." T    = " #T F. ." (" #THETA F. ." - " #ALPHA F. ." )"
      CR ." DIFF = " #DIFF F.
      CR ." TV   = " #TV F.
    THEN

    #DIFF FABS 0.00005e F< IF
      #TV F>S
      #TV #TV F>D D>F F- 60e F* FROUND F>S
      DUP 60 =  IF DROP 1+ 0 THEN
      .HH:MM
      EXIT
    THEN
  AGAIN ;

\ ------------------------------------------------------------------------------

CREATE DAYS 31 , 28 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 ,

: LEAP? ( year -- f )  \ 閏年なら -1 平年なら 0 をスタックに置く
  DUP DUP  4 MOD 0=  SWAP  100 MOD 0<>  AND  SWAP  400 MOD 0=  OR ;

: YM>DAY ( year month -- day )  \ YYYY 年 MM 月 の日数をスタックに置く
  1- DAYS  SWAP  CELLS + @
  DUP 28 =  ROT  LEAP?  AND IF 1+ THEN ;

: APPLY.EVERY-DAY { year xt -- }  \ YYYY-01-01 から YYYY-12-31 まで xt を適用する
  12 1+ 1 DO
    year I YM>DAY 1+ 1 DO
      year J I xt EXECUTE
    LOOP
  LOOP ;

\ ------------------------------------------------------------------------------

: TEST { year month day -- }  \ 日の出時刻を表示する
  day 1 = IF CR CR THEN
  year month day .YYYY/MM/DD ."  -- " year 2000 - month day ITER ." ,  " 
  day 7 MOD 0= IF CR THEN ;

26.2167e 127.6667e SET.LAT/LNG  \ 沖縄の緯度と経度を設定する
2020 ' TEST APPLY.EVERY-DAY

\ ------------------------------------------------------------------------------
