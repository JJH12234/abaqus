C****************************************************
C use Larson_Miller model to calculate Creep Damage
C****************************************************
      pure FUNCTION CRDAMAGE_Larson_Miller(KSAFE, SQ, CTEMP, DURTIME)
      REAL*8,intent(in) :: KSAFE, SQ, CTEMP, DURTIME
      REAL*8 LifePre,aa,bb,cc,C,KTEMP
      REAL*8 X
      REAL*8 CRDAMAGE_Larson_Miller
      REAL*8 LMpara(4)
!larson_miller_start
      LMpara=(/-14120D0, 59180D0, -44700D0,18.26D0/)
!larson_miller_end
      aa = LMpara(1)
      bb = LMpara(2)
      cc = LMpara(3)
      C = LMpara(4)
      X = LOG10(SQ/KSAFE)
      KTEMP = CTEMP+273.15D0
      LifePre = 10**((aa*X**2+bb*X+cc)/KTEMP-C)
C     输出结果
      LifePre = MAX(LifePre, 1.0D-3) !单位h
      CRDAMAGE_Larson_Miller = DURTIME / (LifePre * 3.6D3)  !单位转换，预测寿命h→模型步长s
      END FUNCTION