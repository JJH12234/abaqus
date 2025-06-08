      pure FUNCTION ASME_SQ(CMISES,CSPXY)
          REAL*8,intent(in) :: CMISES, CSPXY(3)
          REAL*8 SQP1, SQP2, Ctri, TRIDEG
          REAL*8 ASME_SQ
          INTEGER MTYPE
          MTYPE={{MTYPE}}
          SQP1=CSPXY(1)+CSPXY(2)+CSPXY(3)
          SQP2=SQRT(CSPXY(1)**2+CSPXY(2)**2+CSPXY(3)**2)
          IF (SQP2.LT.1.0D-8) THEN
              TRIDEG=1.0D0
          ELSE
              TRIDEG=SQP1/SQP2
          ENDIF
          IF (MTYPE.EQ.3) THEN !NH-T-1411 Constant C defined
             IF (TRIDEG.GE.1.0D0) THEN
                 Ctri=0.16D0
             ELSE
                 Ctri=0.0D0
             ENDIF
          ELSEIF (MTYPE.EQ.2) THEN
             Ctri=0.0D0
          ELSEIF (MTYPE.EQ.1) THEN
             Ctri=0.24D0
          ELSE !default
             Ctri={{C}}
          ENDIF !NH-T-1411 Constant C defined
          ASME_SQ=MAX(CMISES*EXP(Ctri*(TRIDEG-1.0D0)),0.0D0)
      END FUNCTION ASME_SQ