

10 V=9*4096
20 V1=V+10:V2=V+11:V3=V+12
30 POKE V+14,15
40 DATA 192,197,200,203,206,208,211,214,216,218,220,222
50 DATA 224,226,227,229,231,232,233,234,236,237,238,239
60 DIM O(2,12)
70 FOR I=1 TO 12:READ O(1,I):NEXT
80 FOR I=1 TO 12:READ O(2,I):NEXT
90 N$="C#D#EF#G#A#B"
100 DIM L$(12)
110 FOR I=1 TO 12:L$(I)=MID$(N$,I,1):NEXT
115 FOR Z=1TO3
120 A$="EDEF":O=1:GOSUB 900
130 A$="G":O=1:GOSUB 900
131 FORL=0TO299:NEXT
140 A$="C":O=1:GOSUB 900
142 FORL=0TO99:NEXT
143 POKEV1,0:POKEV3,0
144 FORL=0TO99:NEXT
145 NEXTZ
150 A$="EDEFD":O=1:GOSUB 900
154 FORL=0TO299:NEXT
800 POKEV1,0:POKEV2,0:POKEV3,0
810 END
900 FOR I=1 TO LEN(A$)
910 C$=MID$(A$,I,1)
920 FOR J=1 TO 12
930 IF C$<>L$(J) THEN NEXT J
940 POKE V3,O(O,J)
941 POKE V1,O(O,J)
950 FOR K=0 TO 100:NEXT K
960 NEXT I
970 RETURN

READY.



READY.


10 V=9*4096
20 V1=V+10:V2=V+11:V3=V+12
30 POKE V+14,15
40 DATA 192,197,200,203,206,208,211,214,216,218,220,222
50 DATA 224,226,227,229,231,232,233,234,236,237,238,239
60 DIM O(2,12)
70 FOR I=1 TO 12:READ O(1,I):NEXT
80 FOR I=1 TO 12:READ O(2,I):NEXT
90 N$="C#D#EF#G#A#B"
100 DIM L$(12)
110 FOR I=1 TO 12:L$(I)=MID$(N$,I,1):NEXT
115 FOR Z=1TO3
120 A$="EDEF":O=1:GOSUB 900
130 A$="G":O=1:GOSUB 900
131 FORL=0TO299:NEXT
140 A$="C":O=1:GOSUB 900
142 FORL=0TO99:NEXT
143 POKEV1,0:POKEV3,0
144 FORL=0TO99:NEXT
145 NEXTZ
150 A$="EDEFD":O=1:GOSUB 900
154 FORL=0TO299:NEXT
800 POKEV1,0:POKEV2,0:POKEV3,0
810 END
900 FOR I=1 TO LEN(A$)
910 C$=MID$(A$,I,1)
920 FOR J=1 TO 12
930 IF C$<>L$(J) THEN NEXT J
940 POKE V3,O(O,J)
941 POKE V1,O(O,J)
950 FOR K=0 TO 100:NEXT K
960 NEXT I
970 RETURN

READY.

