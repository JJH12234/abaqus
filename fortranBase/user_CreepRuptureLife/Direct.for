C****************************************************
C use Direct method to calculate Creep Damage
C****************************************************
      FUNCTION CRDAMAGE_Direct(KSAFE, SQ, CTEMP, DURTIME)
      REAL*8,intent(in) :: KSAFE, SQ, CTEMP, DURTIME
      REAL*8 LifePre,X
      INTEGER Sr_rows
      REAL*8 CRDAMAGE_Direct
      DIMENSION TEMP_list({{TEMP_list}})
      DIMENSION Sr({{rows}},{{columns}})   !寿命-应力数据，rows是表头上的寿命数量
      TEMP_rows = size(TEMP_list,1)
      Sr_rows = size(Sr,1)
!Direct_start
!Direct_end
      X=SQ/KSAFE
      IF(CTEMP.LT.TEMP_list(1))THEN
        LifePre=1E20
      ELSEIF(CTEMP.GT.TEMP_list(TEMP_rows))THEN
        LifePre=0
      ELSE
      DO I=1,TEMP_rows-1,1
        DO J=1,Sr_rows-1,1
          IF(CTEMP.EQ.TEMP_list(I))THEN
            IF(X.LT.Sr(Sr_rows,I+1))THEN
              LifePre=Sr(Sr_rows,1)
		    ELSE IF(X.GT.Sr(1,I+1))THEN
              LifePre=0
            ELSE IF((X.LE.Sr(J,I+1)).AND.(X.GE.Sr(J+1,I+1)))THEN
              LifePre=10.**((LOG10(Sr(J+1,I+1))-LOG10(X)
     1       )/(LOG10(Sr(J+1,I+1))-LOG10(Sr(J,I+1)))*LOG10(Sr(J,1)
     1       )+(LOG10(X)-LOG10(Sr(J,I+1)))/(LOG10(Sr(J+
     1      1,I+1))-LOG10(Sr(J,I+1)))*LOG10(Sr(J+1,1)))
            END IF
          END IF

          IF((CTEMP.GT.TEMP_list(I)).AND.(CTEMP.LE.TEMP_list(I+1)))THEN
            IF(X.LT.Sr(Sr_rows,I+2))THEN
              LifePre=Sr(Sr_rows,1)
            ELSE IF(X.GT.Sr(1,I+2))  THEN
              LifePre=0
            ELSE IF((X.LE.Sr(J,I+2)).AND.(X.GE.Sr(J+1,I+2)))THEN
              LifePre=10.**((LOG10(Sr(J+1,I+2))-LOG10(X)
     1       )/(LOG10(Sr(J+1,I+2))-LOG10(Sr(J,I+2)))*LOG10(Sr(J,1)
     1       )+(LOG10(X)-LOG10(Sr(J,I+2)))/(LOG10(Sr(J+
     1       1,I+2))-LOG10(Sr(J,I+2)))*LOG10(Sr(J+1,1)))
            END IF
          END IF
        END DO
      END DO
      END IF
C     输出结果
      LifePre = MAX(LifePre, 1.0D-3) !单位h
      CRDAMAGE_Direct = DURTIME / (LifePre*3600D0) !单位转换，预测寿命h→模型步长s
      END FUNCTION CRDAMAGE_Direct