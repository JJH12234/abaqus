      SUBROUTINE CREEP(DECRA,DESWA,STATEV,SERD,EC,ESW,P,QTILD,
     1 TEMP,DTEMP,PREDEF,DPRED,TIME,DTIME,CMNAME,LEXIMP,LEND,
     2 COORDS,NSTATV,NOEL,NPT,LAYER,KSPT,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
C
      DIMENSION DECRA(5),DESWA(5),STATEV(*),PREDEF(*),DPRED(*),
     1 TIME(3),EC(2),ESW(2),COORDS(*)
      REAL*8, DIMENSION(5) :: DECRAT,DECRAT1,DECRAT2!插值用应变变量 对标DECRA
      INTEGER :: rowIndex1, rowIndex2
      DIMENSION ARRAY(15), JARRAY(15)
      CHARACTER*3 FLGRAY(15)
      CHARACTER*6 :: FLAG
      REAL*8 T1,T2,T0
      REAL*8 W
      REAL*8 :: timescale=1.0D0/3600.0D0 !模型time单位*scale→本构time单位
C 输入蠕变参数!蠕变本构方程参数（以q0为形式，避免E-27）
!CREEP_PARAMS_START
      
      REAL*8, DIMENSION(7, 5) :: CreepCEpara_q0= RESHAPE(
     1 (/400D0, 1D0, 399.6133473D0, 23.08D0, -0.6733D0,
     2 394.3857269D0, 57.86D0,
     3 450D0, 1D0, 403.6786758D0, 11.56D0, -0.598D0,
     4 408.2303045D0, 18.89D0,
     5 482D0, 1D0, 592.3749238D0, 4.089D0, -0.5399D0,
     6 501.3181752D0, 7.933D0,
     7 510D0, 1D0, 544.7206486D0, 3.682D0, -0.4937D0,
     8 382.1866435D0, 8.508D0,
     9 538D0, 1D0, 382.5080257D0, 4.128D0, -0.4519D0,
     1 294.4888176D0, 9.214D0/),
     2 (/7, 5/))
!CREEP_PARAMS_END
C     REAL*8, DIMENSION(7, 7) :: CreepParaList
      REAL*8, DIMENSION(7) :: CreepPara!传入蠕变参数
      DECRA=0.0D0 !初始化
      DECRAT=0.0D0 !初始化
          rowIndex1=count(CreepCEpara_q0(1,:).LE.TEMP)
          IF (TEMP.LT.CreepCEpara_q0(1,1)) THEN !温度小于最小
              DECRA=0.0D0
          ELSEIF (COUNT(CreepCEpara_q0(1,:).eq.TEMP).EQ.1.OR.
     1     COUNT(CreepCEpara_q0(1,:).eq.TEMP).EQ.2) then !温度恰好等于
              CreepPara=CreepCEpara_q0(:,rowIndex1)
              Call CreepRCC_q0(DECRA,EC,QTILD,TIME,DTIME,LEXIMP,
     1            CreepPara,timescale)
          ELSEIF (TEMP.GT.maxval(CreepCEpara_q0(1,:))) THEN !温度大于最后
              CreepPara=reshape(CreepCEpara_q0(:,maxloc(CreepCEpara_q0(1,:))),(/7/))
              Call CreepRCC_q0(DECRA,EC,QTILD,TIME,DTIME,LEXIMP,
     1            CreepPara,timescale)
          ELSE 
                  rowIndex2=rowIndex1+1
                  CreepPara=CreepCEpara_q0(:,rowIndex1)
                  Call CreepRCC_q0(DECRA,EC,QTILD,TIME,DTIME,LEXIMP,
     1               CreepPara,timescale)
                  CreepPara=CreepCEpara_q0(:,rowIndex2)
                  Call CreepRCC_q0(DECRAT,EC,QTILD,TIME,DTIME,LEXIMP,
     1               CreepPara,timescale)
              !对DECRA(1,5)使用1/T插值 应力插值结果根据温度插值
              T1=CreepCEpara_q0(1,rowIndex1)+273.15D0
              T2=CreepCEpara_q0(1,rowIndex2)+273.15D0
              T0=TEMP+273.15D0
              W=(1.0D0/T0-1.0D0/T1)/(1.0D0/T2-1.0D0/T1)
              DECRA(1)=DECRAT(1)**W*DECRA(1)**(1.0D0-W)
              DECRA(5)=DECRAT(5)**W*DECRA(5)**(1.0D0-W)
          ENDIF
      DECRA=DECRA*0.01D0 ! 返回最终结果前，%应变改为1
      RETURN
      END 


C***********Creep Constitutive Equation***********
!CreepCE_start

      SUBROUTINE CreepRCC_q0(DECRA,EC,QTILD,TIME,DTIME,
     1 LEXIMP,RCCpara,timescale)
      DIMENSION DECRA(5),TIME(3),EC(2),RCCpara(7)
      INTEGER,intent(in) :: LEXIMP
      REAL*8,intent(in) :: DTIME,QTILD,EC,TIME,timescale
      REAL*8 DECRA
      REAL*8 dep0,q0,q1,n1,Q,QQ,A,N,M,XM,P1,P3,tfp,efp,RCCpara
      IF (QTILD.GT.1.0D-6) THEN
          dep0=RCCpara(2)
          q0=RCCpara(3)
          N=RCCpara(4)
          M=RCCpara(5)
          q1=RCCpara(6)
          n1=RCCpara(7)
          XM=1.0D0/(M+1.0D0)
          A=dep0**(M+1.0D0) !非原有A
          A1=dep0 !非原有A1
          Q=QTILD/q0
          QQ=QTILD/q1
          tfp=(QQ**n1/Q**n)**(1.0D0/M) !本质是应变速率一阶段=二阶段解出的解
          efp=A*XM*tfp**(m+1.0D0)*Q**n
          IF ((TIME(3)*timescale).LE.tfp!.AND.EC(1)*100.0D0.LE.efp
     1         .OR.isNaN(tfp).OR.isNaN(efp)) then !一阶段
              P1=A*Q**N*XM
              P3=(TIME(3)*timescale)**(M+1.0D0)-(TIME(3)*timescale-DTIME*timescale)**(M+1.0D0)
              DECRA(1)=P1*P3
              IF (LEXIMP.EQ.1) THEN
                  DECRA(5)=A*N*XM*Q**(N-1.0D0)/q0*P3
              ENDIF
          elseIF ((TIME(3)-dtime)*timescale.GE.tfp) then !纯粹二阶段
              DECRA(1)=A1*QQ**n1*DTIME*timescale
              IF (LEXIMP.EQ.1) THEN
                  DECRA(5)=A1*N1*QQ**(N1-1.0D0)/q1*DTIME*timescale
              ENDIF
          ELSEIF ((TIME(3)-dtime)*timescale.lt.tfp
     1        .and.(TIME(3)*timescale).GT.tfp) then !过渡二阶段
              P3=tfp**(M+1.0D0)-((TIME(3)-DTIME)*timescale)**(M+1.0D0)
              DECRA(1)=A*Q**N*XM*P3+A1*QQ**n1*((TIME(3))*timescale-tfp)
              IF (LEXIMP.EQ.1) THEN
                  DECRA(5)=A*N*Q**(N-1.0D0)/q0*XM*P3
     1                 +A1*N1*QQ**(N1-1.0D0)/q1*((TIME(3))*timescale-tfp)
              ENDIF
          endIF
      else
          DECRA(1)=0.0
          IF (LEXIMP.EQ.1) THEN
              DECRA(5)=0.0
          endif
      endif
      RETURN
      END


!CreepCE_end

      
      
      SUBROUTINE UVARM(UVAR,DIRECT,T,TIME,DTIME,CMNAME,ORNAME,
     1 NUVARM,NOEL,NPT,LAYER,KSPT,KSTEP,KINC,NDI,NSHR,COORD,
     2 JMAC,JMATYP,MATLAYO,LACCFLA) 
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3 FLGRAY(15)
      DIMENSION UVAR(NUVARM),DIRECT(3,3),T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),COORD(*)
      integer GAP,MTYPE,ignoreStep,NResetStep
      integer FLAGforDo
      REAL*8 :: DE(6)
      REAL*8 DEQP1,DEQP2,DEQ,SIGNCORRECT
      REAL*8 RepSQ,CTEMP,CMISES,SF
      REAL*8 CSPXY(3)!,CSXY(3)
      REAL*8 TempGAP,StressGAP,Time1forFatigue
      REAL*8 DTEMP,REPDS,DURTIME,MinCreepTemp
      REAL*8 StressThreshold,TempThreshold,Vstar
      REAL*8,external :: ASME_SQ,RCC_SQ,CRDAMAGE_Direct,is_above
      REAL*8,external :: loglog_interpolate_Direct
C USER DEFEND TOP
      StressThreshold=1.0D0 !Use for CreepDamage Integral
      TempThreshold=15.0D0 !Use for CreepDamage Integral
      Vstar=0.5D0 !Vstar for Ela is 0.3; Inela is 0.5 !考虑到计算效率暂时没有用上
      SF=0.9
      ignoreStep={{ignoreStep}}     !不参与疲劳蠕变计算的分析步
      NResetStep={{NResetStep}}     !循环节长度
      Time1forFatigue={{Time1forFatigue}}   !循环最后一分析步最终时刻
      MinCreepTemp={{MinCreepTemp}}         !蠕变门槛温度
C USER DEFEND BOTTOM
C FatigueStrainRangeCommand top
      IF (KSTEP.EQ.1.AND.KINC.EQ.0) THEN !Initial步 重置预设值
          UVAR(1)=-huge(UVAR(1))
          UVAR(2)=huge(UVAR(2))
          DEQP1=0.0D0
          DEQP2=0.0D0
          DEQ=0.0D0
          DE=0D0
          SIGNCORRECT=0.0D0
          UVAR(31)=0d0
      ENDIF
      IF (KSTEP>ignoreStep) THEN 
          IF (mod(KSTEP-ignoreStep,NResetStep)==1.AND.KINC==1) then !每个循环第一步的第一个增量步 重置预设值 并传递前一循环应变分量
          UVAR(3:8)=UVAR(18:23)
          UVAR(9:14)=UVAR(18:23)!为了补充Inc1~Inc0的应变范围，迫不得已多申请了7个变量……
          UVAR(1)=UVAR(24)
          UVAR(2)=UVAR(24)
          UVAR(16)=-huge(UVAR(16))
          FLAGforDO=1
          ENDIF
      call getvrm('EP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
      SIGNCORRECT=SIGN(1.0D0,ARRAY(1)+ARRAY(2)+ARRAY(3))
      call getvrm('E',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
      DEQP1=(array(1)-array(2))**2.0D0+(array(2)-array(3))**2.0D0+
     1 (array(3)-array(1))**2.0D0
      DEQP2=array(4)**2.0D0+array(5)**2.0D0+array(6)**2.0D0           !假设E4~6都是工程剪应变γ
      DEQ=0.47140452079103173D0*SQRT(DEQP1+DEQP2*1.5D0)*SIGNCORRECT   !Vstar is 0.5
      UVAR(18:23)=array(1:6)
      UVAR(24)=DEQ
          IF (DEQ.GE.UVAR(1)) THEN !Current DEQ is bigger, save the E11~E33
              UVAR(1)=DEQ
              UVAR(3:8)=array(1:6)
              FLAGforDo=1
          ENDIF
          IF (DEQ.LE.UVAR(2)) THEN !Current DEQ is littler, save the E11~E33
              UVAR(2)=DEQ
              UVAR(9:14)=array(1:6)
              FLAGforDo=1
          ENDIF
!Calculate the range between bigger and littler
          IF (FLAGforDo.EQ.1) THEN
          FLAGforDo=0
          DE(1:6)=UVAR(3:8)-UVAR(9:14)
          DEQP1=(DE(1)-DE(2))**2.0D0+(DE(2)-DE(3))**2.0D0+
     1      (DE(3)-DE(1))**2.0D0
          DEQP2=DE(4)**2.0D0+DE(5)**2.0D0+DE(6)**2.0D0          !假设E4~6都是工程剪应变γ
          UVAR(15)=0.47140452079103173D0*SQRT(DEQP1+DEQP2*1.5)  !Vstar is 0.5
            IF (UVAR(15).GT.UVAR(16)) THEN
            UVAR(16)=UVAR(15)
            ENDIF
          ENDIF
	ELSE !如果是ignoreStep，仅保存应变分量和当量，不计算U15 U16
      call getvrm('EP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
      SIGNCORRECT=SIGN(1.0D0,ARRAY(1)+ARRAY(2)+ARRAY(3))
      call getvrm('E',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
      DEQP1=(array(1)-array(2))**2D0+(array(2)-array(3))**2.0D0+
     1 (array(3)-array(1))**2D0
      DEQP2=array(4)**2.0D0+array(5)**2.0D0+array(6)**2.0D0 !假设E4~6都是工程剪应变γ
      DEQ=0.47140452079103173D0*SQRT(DEQP1+DEQP2*1.5D0)*SIGNCORRECT !Vstar is 0.5
      UVAR(18:23)=array(1:6)
      UVAR(24)=DEQ
      ENDIF
C**********FatigueStrainRangeCommand Bottom**********      
C**********CreepDamageCommand Top**********
      IF (KSTEP==1.AND.KINC==1) THEN
          call getvrm('SINV',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
          CMISES=ARRAY(1)
          call getvrm('TEMP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
          CTEMP=ARRAY(1)
          call getvrm('SP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
          CSPXY=(/ARRAY(1),ARRAY(2),ARRAY(3)/)
          UVAR(30)=(CSPXY(1)+CSPXY(2)+CSPXY(3))/
     1      SQRT(CSPXY(1)**2+CSPXY(2)**2+CSPXY(3)**2)
C          call getvrm('S',array,jarray,flgray,jrcd,jmac,jmatyp,
C     1 matlayo,laccfla)
C          CSXY=(/ARRAY(1),ARRAY(2),ARRAY(3)/)
          REPSQ=ASME_SQ(CMISES,CSPXY)    !前端传入选用的等效应力方法(ASME,RCC),REP_SQ是函数,REPSQ是等效应力
          UVAR(17)=CTEMP
          UVAR(25)=REPSQ
      ELSEIF (KSTEP>1.OR.KINC>1) THEN !当不是STEP 1 INC 0/1的时候
          call getvrm('SINV',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
          CMISES=ARRAY(1)
          call getvrm('TEMP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
          CTEMP=ARRAY(1)
          call getvrm('SP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1 matlayo,laccfla)
          CSPXY=(/ARRAY(1),ARRAY(2),ARRAY(3)/)
          UVAR(30)=(CSPXY(1)+CSPXY(2)+CSPXY(3))/
     1      SQRT(CSPXY(1)**2+CSPXY(2)**2+CSPXY(3)**2)
          REPSQ=ASME_SQ(CMISES,CSPXY)
          IF (CTEMP>MinCreepTemp) THEN                          !计算蠕变损伤
              TempGAP=abs(CTEMP-UVAR(17))/TempThreshold         !两步温度差决定的插值间隔
              StressGAP=abs(REPSQ-UVAR(25))/StressThreshold     !两步蠕变损伤用等效应力的差决定的插值间隔
              GAP=MAX(int(TempGAP),int(StressGAP),1)
              DURTIME=DTIME/GAP
              do i=1,GAP
                DTEMP=CTEMP*(i-0.5D0)/GAP+UVAR(17)*(1.0D0-(i-0.5D0)/GAP)
                REPDS=REPSQ*(i-0.5D0)/GAP+UVAR(25)*(1.0D0-(i-0.5D0)/GAP)
                UVAR(27)=UVAR(27)+CRDAMAGE_Direct(SF,REPDS,CTEMP,DURTIME)
C 经测试SF09和SF67这些位置直接使用常数会导致并行求解错误                  
              enddo
          ENDIF
          UVAR(17)=CTEMP
          UVAR(25)=REPSQ    !在求完损伤后,赋值UVAR(25)为当前增量步下的蠕变损伤用等效应力
      ENDIF
C**********CreepDamageCommand Bottom**********
C**********FatigueDamage Top**********
      IF (Time(1)==Time1forFatigue.and. !循环最后一分析步时间恰好为指定值时（用来判断最后一增量步）
     1 mod(KSTEP-ignoreStep,NResetStep)==0.and. !每个循环最后一分析步
     2 UVAR(16)>0) THEN !应变范围不为0
      UVAR(31)=UVAR(31)+
     1  1.0D0/loglog_interpolate_Direct(UVAR(16),UVAR(17))
      ENDIF
C**********FatigueDamage Bottom**********   !使用交互作用图确定蠕变-疲劳损伤
      UVAR(32)=is_above(UVAR(31),UVAR(27))
      RETURN
      END

C***********REP_SQ***********
!REP_SQ_start
      pure FUNCTION ASME_SQ(CMISES,CSPXY)
          REAL*8,intent(in) :: CMISES, CSPXY(3)
          REAL*8 SQP1, SQP2, Ctri, TRIDEG
          REAL*8 ASME_SQ
          INTEGER MTYPE
          MTYPE=3
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
             Ctri=0.24
          ENDIF !NH-T-1411 Constant C defined
          ASME_SQ=MAX(CMISES*EXP(Ctri*(TRIDEG-1.0D0)),0.0D0)
      END FUNCTION ASME_SQ
!REP_SQ_end
      
C***********Creep Damage Calculate***********
!Creep_Damage_start
C****************************************************
C use Direct method to calculate Creep Damage
C****************************************************
      FUNCTION CRDAMAGE_Direct(KSAFE, SQ, CTEMP, DURTIME)
      REAL*8,intent(in) :: KSAFE, SQ, CTEMP, DURTIME
      REAL*8 LifePre,X
      INTEGER Sr_rows
      REAL*8 CRDAMAGE_Direct
      DIMENSION TEMP_list(11)
      DIMENSION Sr(10,12)   !寿命-应力数据，rows是表头上的寿命数量
      TEMP_rows = size(TEMP_list,1)
      Sr_rows = size(Sr,1)
!Direct_start

      DATA TEMP_list/371D0, 399D0, 427D0, 454D0, 482D0,
     1 510D0, 538D0, 566D0, 593D0, 621D0,
     2 649D0/
      DATA Sr/10D0, 30D0, 100D0, 300D0, 1000D0,
     1 3000D0, 10000D0, 30000D0, 100000D0, 300000D0,
     2 406.8D0, 406.8D0, 406.8D0, 406.8D0, 406.8D0,
     3 406.8D0, 406.8D0, 406.8D0, 372.3D0, 337.8D0,
     4 399.9D0, 393.0D0, 386.1D0, 376.5D0, 365.4D0,
     5 353.0D0, 330.9D0, 298.5D0, 258.6D0, 235.1D0,
     6 386.1D0, 382.7D0, 372.3D0, 334.4D0, 296.5D0,
     7 258.6D0, 237.9D0, 210.3D0, 186.2D0, 165.5D0,
     8 358.5D0, 348.2D0, 317.2D0, 279.2D0, 241.3D0,
     9 213.7D0, 189.6D0, 165.5D0, 144.8D0, 127.6D0,
     1 317.2D0, 282.7D0, 248.2D0, 220.6D0, 193.1D0,
     2 172.4D0, 148.9D0, 131.0D0, 113.1D0, 97.2D0,
     3 275.8D0, 241.3D0, 206.8D0, 179.3D0, 153.1D0,
     4 134.4D0, 117.2D0, 100.7D0, 86.9D0, 75.8D0,
     5 217.2D0, 189.6D0, 165.5D0, 144.8D0, 123.4D0,
     6 104.8D0, 90.3D0, 75.8D0, 64.8D0, 54.5D0,
     7 179.3D0, 155.1D0, 131.0D0, 113.8D0, 96.5D0,
     8 82.7D0, 68.9D0, 57.2D0, 48.3D0, 40.0D0,
     9 144.8D0, 124.1D0, 104.1D0, 89.6D0, 74.5D0,
     1 62.7D0, 51.7D0, 42.7D0, 34.5D0, 28.3D0,
     2 117.2D0, 97.2D0, 81.4D0, 67.6D0, 55.2D0,
     3 1D0, 1D0, 1D0, 1D0, 1D0,
     4 93.1D0, 76.5D0, 63.4D0, 52.4D0, 42.7D0,
     5 1D0, 1D0, 1D0, 1D0, 1D0/
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
!Creep_Damage_end
      

C***********Fatigue Life Calculate***********
!Fatigue_life_model_start
C****************************************************
C use Direct(chazhi) model to calculate Nf
C****************************************************
      function loglog_interpolate_Direct(U16,maxTEMP)
        implicit none
        ! Input parameters
        real*8 :: strain_range(16)   ! 单调递减的应变范围
        real*8 :: life_data(16)      ! 单调递增的life
        real*8 :: temps(7)
        real*8, intent(in) :: U16,maxTEMP              ! 给定的应变范围和最高温度
!Fatigue_life_Directmodel_start
      REAL*8, DIMENSION(8, 17) :: SN= RESHAPE(
     1 (/0D0, 40D0, 425D0, 480D0, 540D0,
     2 595D0, 650D0, 705D0,
     3 10D0, 0.051D0, 0.05D0, 0.0465D0, 0.0425D0,
     4 0.0382D0, 0.0335D0, 0.0297D0,
     5 20D0, 0.036D0, 0.0345D0, 0.0315D0, 0.0284D0,
     6 0.025D0, 0.0217D0, 0.0186D0,
     7 40D0, 0.0263D0, 0.0246D0, 0.0222D0, 0.0197D0,
     8 0.017D0, 0.0146D0, 0.0123D0,
     9 100D0, 0.018D0, 0.0164D0, 0.0146D0, 0.0128D0,
     1 0.011D0, 0.0093D0, 0.0077D0,
     2 200D0, 0.0142D0, 0.0125D0, 0.011D0, 0.0096D0,
     3 0.0082D0, 0.0069D0, 0.0057D0,
     4 400D0, 0.0113D0, 0.00965D0, 0.00845D0, 0.00735D0,
     5 0.0063D0, 0.00525D0, 0.00443D0,
     6 1000D0, 0.00845D0, 0.00725D0, 0.0063D0, 0.0055D0,
     7 0.0047D0, 0.00385D0, 0.00333D0,
     8 2000D0, 0.0067D0, 0.0059D0, 0.0051D0, 0.0045D0,
     9 0.0038D0, 0.00315D0, 0.00276D0,
     1 4000D0, 0.00545D0, 0.00485D0, 0.0042D0, 0.00373D0,
     2 0.0032D0, 0.00263D0, 0.0023D0,
     3 10000D0, 0.0043D0, 0.00385D0, 0.00335D0, 0.00298D0,
     4 0.0026D0, 0.00215D0, 0.00185D0,
     5 20000D0, 0.0037D0, 0.0033D0, 0.0029D0, 0.00256D0,
     6 0.00226D0, 0.00187D0, 0.00158D0,
     7 40000D0, 0.0032D0, 0.00287D0, 0.00254D0, 0.00224D0,
     8 0.00197D0, 0.00162D0, 0.00138D0,
     9 100000D0, 0.00272D0, 0.00242D0, 0.00213D0, 0.00188D0,
     1 0.00164D0, 0.0014D0, 0.00117D0,
     2 200000D0, 0.0024D0, 0.00215D0, 0.0019D0, 0.00167D0,
     3 0.00145D0, 0.00123D0, 0.00105D0,
     4 400000D0, 0.00215D0, 0.00192D0, 0.0017D0, 0.0015D0,
     5 0.0013D0, 0.0011D0, 0.00094D0,
     6 1000000D0, 0.0019D0, 0.00169D0, 0.00149D0, 0.0013D0,
     7 0.00112D0, 0.00098D0, 0.00084D0/),
     9 (/8, 17/))
!Fatigue_life_Directmodel_end
        ! Output result
        real*8 :: loglog_interpolate_Direct
        ! Local variables
        integer :: idx, SN_rows, SN_cols, idx_low, idx_high
        real*8 :: logU16, log_strain1, log_strain2
        real*8 :: log_life1, log_life2, t1, t2, ratio
        real*8 :: slope, i
C
        SN_rows = size(SN,1)
        SN_cols = size(SN,2)

        ! 判断温度处于哪个区间
        if (maxTEMP <= SN(2,1)) then
            strain_range = SN(2,2:SN_cols)  ! 使用最小温度数据
        elseif (maxTEMP >= SN(SN_rows,1)) then
            strain_range = SN(SN_rows,2:SN_cols)  ! 使用最大温度数据
        else
            idx_low = 0   ! 初始化边界
            idx_high = 0
            temps = SN(2:SN_rows,1) !从SN中取出温度，第1列的第2行到最底行
            ! 遍历温度数组
            do i = 1, SN_rows-1
              if (temps(i) <= maxTEMP .and. maxTEMP <= temps(i+1)) then
                idx_low = i
                idx_high = i+1
                exit
              end if
            end do
            ! 确定了插值温度的区间
            t1 = temps(idx_low)
            t2 = temps(idx_high)
            ratio = (maxTEMP - t1)/(t2 - t1)
            do i = 2, SN_cols
                strain_range(i-1) = SN(idx_low+1,i)
     1            + ratio*(SN(idx_high+1,i)-SN(idx_low+1,i))
            end do
        end if
C
        ! 转换给定应变范围的对数
        logU16 = log10(U16)
        life_data=SN(1,2:SN_cols)
        ! 判断应变是否超出范围
        if (U16 >= strain_range(1)) then
            loglog_interpolate_Direct = life_data(1)
            return
        elseif (U16 <= strain_range(SN_cols-1)) then
            loglog_interpolate_Direct = life_data(SN_cols-1)
            return
        endif
        ! 使用count函数判断插值区间
        idx = count(strain_range > U16)
        ! idx为插值区间的左端点，右端点为idx+1
        ! 进行对数插值
        log_strain1 = log10(strain_range(idx))
        log_strain2 = log10(strain_range(idx + 1))
        log_life1 = log10(life_data(idx))
        log_life2 = log10(life_data(idx + 1))
        ! 计算斜率
        slope = (log_life2 - log_life1) / (log_strain2 - log_strain1)
        ! 插值计算
        loglog_interpolate_Direct = 10.0D0**
     1      (log_life1 + slope * (logU16 - log_strain1))
      end function loglog_interpolate_Direct
!Fatigue_life_model_end


C***********Creep-Fatigue Enevlope***********
!Creep_Fatigue_Enevlope_start
C****************************************************
C Creep-Fatigue Enevlope
C****************************************************
      function is_above(Fatigue,Creep)
        implicit none
        real*8, intent(in) :: Fatigue,Creep
        real*8 :: is_above
        real*8 :: y,k,b,u,s
        is_above = 0.0d0
        if (Creep>=1d0.or.Fatigue>=1d0) then
            is_above=1.0d0
            return
        elseif (Fatigue==0.0d0) then
            is_above = Creep/1.0d0
            return
        elseif (Creep==0.0d0) then
            is_above = Fatigue/1.0d0
            return
        !elseif (Creep <= 0.1d0) then
        elseif (1.0D0*Fatigue>Creep) then
            k=-0.428571428571D0        !-9.0d0
            b=0.428571428571D0        !1.0d0
        !elseif (Creep > 0.1d0) then
        elseif (Creep>=1.0D0*Fatigue) then
            k=-2.33333333333D0        !-1.0d0/9.0d0
            b=1.0D0        !1.0d0/9.0d0
        endif
        y=k*Creep+b
        if (Fatigue > y) then
            is_above = 1.0d0
            return
        else
            u=Fatigue/Creep-k
            s=b/u*sqrt(1d0+(k+u)**2d0)
            is_above=sqrt(Creep**2d0+Fatigue**2d0)/s
        endif
      end function is_above
!Creep_Fatigue_Enevlope_end