      SUBROUTINE CREEP(DECRA,DESWA,STATEV,SERD,EC,ESW,P,QTILD,
     1TEMP,DTEMP,PREDEF,DPRED,TIME,DTIME,CMNAME,LEXIMP,LEND,
     2COORDS,NSTATV,NOEL,NPT,LAYER,KSPT,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
C
      DIMENSION DECRA(5),DESWA(5),STATEV(*),PREDEF(*),DPRED(*),
     1TIME(3),EC(2),ESW(2),COORDS(*)
      REAL*8, DIMENSION(5) :: DECRAT,DECRAT1,DECRAT2!插值用应变变量 对标DECRA
      INTEGER :: rowIndex1, rowIndex2
      DIMENSION ARRAY(15), JARRAY(15)
      CHARACTER*3 FLGRAY(15)
      CHARACTER*6 :: FLAG
      REAL*8 T1,T2,T0
      REAL*8 W
      REAL*8 :: timescale=1.0D0/3600.0D0 !模型time单位*scale→本构time单位
      REAL*8 t_ratio, DTIME_above, T_avg
      REAL*8, DIMENSION(5) :: DECRA_above
      REAL*8, DIMENSION(3) :: TIME_above
      REAL*8, DIMENSION(2) :: EC_above
      REAL*8, DIMENSION(7) :: CreepPara_above
      INTEGER :: rowIndex1a, rowIndex2a
      REAL*8 T_lower, T_upper, T_start, T_end
      LOGICAL :: heating
      integer :: idx_min(1), idx_max(1)
C 输入蠕变参数!蠕变本构方程参数（以q0为形式，避免E-27）
!CREEP_PARAMS_START
      REAL*8, DIMENSION({0}, {1}) :: CreepCEpara_q0= RESHAPE(
!CREEP_PARAMS_END
C     REAL*8, DIMENSION(7, 7) :: CreepParaList
      REAL*8, DIMENSION(7) :: CreepPara!传入蠕变参数
      DECRA=0.0D0 !初始化
      DECRAT=0.0D0 !初始化
      T_lower = CreepCEpara_q0(1,1)  ! 临界温度
      T_avg = TEMP - DTEMP / 2.0D0
      idx_min=minloc(CreepCEpara_q0(1,:))
      idx_max=maxloc(CreepCEpara_q0(1,:))
      IF (MAX(TEMP,TEMP-DTEMP)<T_lower) then
          DECRA=0d0
      ! 处理温度跨越的情况 (升温和降温)
      ELSEIF (((TEMP >= T_lower) .AND. ((TEMP - DTEMP) < T_lower)) .OR.
     1((TEMP < T_lower) .AND. ((TEMP - DTEMP) >= T_lower))) THEN
      ! 计算跨越比例
              ! 初始化临时变量
              DECRA_above = 0.0D0
              TIME_above = TIME
              EC_above = EC
              heating = (TEMP > (TEMP - DTEMP))! 确定温度变化方向
              IF (heating) THEN
              ! 升温过程
                  t_ratio = (TEMP - T_lower) / DTEMP
                  T_start = T_lower
                  T_end = TEMP
                  DTIME_above = t_ratio * DTIME
                  TIME_above(3) = TIME(3)  ! 调整时间点
              ELSE
              ! 降温过程
                  t_ratio = ((TEMP - DTEMP) - T_lower) / (-DTEMP)
                  T_start = TEMP - DTEMP
                  T_end = T_lower
                  DTIME_above = t_ratio * DTIME
                  TIME_above(3) = TIME(3) - DTIME + DTIME_above
              ENDIF
              T_avg = (T_start + T_end) / 2.0D0
              ! 获取平均温度对应的蠕变参数
              IF (T_avg .LT. T_lower) THEN !使用第一行
                  CreepPara_above = CreepCEpara_q0(:,idx_min(1))
                  CALL {{CreepCE_X}}(DECRA_above, EC_above, QTILD,
     1              TIME_above, DTIME_above, LEXIMP, CreepPara_above,
     2              timescale)
              ELSEIF (T_avg .GT. MAXVAL(CreepCEpara_q0(1,:))) THEN !使用最后一行
                  CreepPara_above = CreepCEpara_q0(:,idx_max(1))
                  CALL {{CreepCE_X}}(DECRA_above, EC_above, QTILD,
     1              TIME_above, DTIME_above, LEXIMP, CreepPara_above,
     2              timescale)
              ELSEIF (COUNT(CreepCEpara_q0(1,:) .EQ. T_avg) .GE. 1) THEN !使用对应行
                  rowIndex1a = COUNT(CreepCEpara_q0(1,:) .LE. T_avg)
                  CreepPara_above = CreepCEpara_q0(:, rowIndex1a)
                  CALL {{CreepCE_X}}(DECRA_above, EC_above, QTILD,
     1              TIME_above, DTIME_above, LEXIMP, CreepPara_above,
     2              timescale)
              ELSE
                  rowIndex1a = COUNT(CreepCEpara_q0(1,:) .LE. T_avg)
                  rowIndex2a = rowIndex1a + 1
                  CALL {{CreepCE_X}}(DECRA1, EC_above, QTILD,
     1              TIME_above, DTIME_above, LEXIMP,
     2              CreepCEpara_q0(:, rowIndex1a), timescale)
                  CALL {{CreepCE_X}}(DECRA2, EC_above, QTILD,
     1              TIME_above, DTIME_above, LEXIMP,
     2              CreepCEpara_q0(:, rowIndex2a), timescale)
                  ! 1/T插值
                  T1 = CreepCEpara_q0(1, rowIndex1a) + 273.15D0
                  T2 = CreepCEpara_q0(1, rowIndex2a) + 273.15D0
                  T0 = T_avg + 273.15D0
                  W = (1.0D0/T0 - 1.0D0/T1) / (1.0D0/T2 - 1.0D0/T1)
                  DECRA_above(1) = DECRA2(1)**W * DECRA1(1)**(1.0D0 - W)
                  IF (LEXIMP .EQ. 1) THEN
                      DECRA_above(5) = DECRA2(5)**W * DECRA1(5)**(1.0D0 - W)
                  ENDIF
              ENDIF
              DECRA(1) = DECRA_above(1) 
              IF (LEXIMP .EQ. 1) THEN
                  DECRA(5) = DECRA_above(5)
              ENDIF
              ! 原有温度处理逻辑
      ELSEIF (COUNT(CreepCEpara_q0(1,:).eq.T_avg).EQ.1) then !温度恰好等于
              rowIndex1=count(CreepCEpara_q0(1,:).LE.T_avg)
              CreepPara=CreepCEpara_q0(:,rowIndex1)
              Call {{CreepCE_X}}(DECRA,EC,QTILD,TIME,DTIME,LEXIMP,
     1          CreepPara,timescale)
      ELSEIF (T_avg.GT.maxval(CreepCEpara_q0(1,:))) THEN !温度大于最后
              CreepPara_above = CreepCEpara_q0(:,idx_max(1))
              Call {{CreepCE_X}}(DECRA,EC,QTILD,TIME,DTIME,LEXIMP,
     1          CreepPara,timescale)
      ELSE
              rowIndex1=count(CreepCEpara_q0(1,:).LE.T_avg)
              rowIndex2=rowIndex1+1
              CreepPara=CreepCEpara_q0(:,rowIndex1)
              Call {{CreepCE_X}}(DECRA,EC,QTILD,TIME,DTIME,LEXIMP,
     1          CreepPara,timescale)
              CreepPara=CreepCEpara_q0(:,rowIndex2)
              Call {{CreepCE_X}}(DECRAT,EC,QTILD,TIME,DTIME,LEXIMP,
     1          CreepPara,timescale)
              !对DECRA(1,5)使用1/T插值 应力插值结果根据温度插值
              T1=CreepCEpara_q0(1,rowIndex1)+273.15D0
              T2=CreepCEpara_q0(1,rowIndex2)+273.15D0
              T0=T_avg+273.15D0
              W=(1.0D0/T0-1.0D0/T1)/(1.0D0/T2-1.0D0/T1)
              DECRA(1)=DECRAT(1)**W*DECRA(1)**(1.0D0-W)
              DECRA(5)=DECRAT(5)**W*DECRA(5)**(1.0D0-W)
      ENDIF
      DECRA=DECRA*0.01D0 ! 返回最终结果前，%应变改为1
      RETURN
      END


C***********Creep Constitutive Equation***********
!CreepCE_start
!CreepCE_end



      SUBROUTINE UVARM(UVAR,DIRECT,T,TIME,DTIME,CMNAME,ORNAME,
     1NUVARM,NOEL,NPT,LAYER,KSPT,KSTEP,KINC,NDI,NSHR,COORD,
     2JMAC,JMATYP,MATLAYO,LACCFLA)
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
      REAL*8,external :: ASME_SQ,RCC_SQ,{{CRDAMAGE}},is_above
      REAL*8,external :: loglog_interpolate_{{user_FatigueLife_method}}
      INTEGER :: i
C USER DEFEND TOP
      StressThreshold=1.0D0 !Use for CreepDamage Integral
      TempThreshold=15.0D0 !Use for CreepDamage Integral
      Vstar=0.5D0 !Vstar for Ela is 0.3; Inela is 0.5 !考虑到计算效率暂时没有用上
      SF={{SF}}
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
          UVAR(33)=-huge(UVAR(16))
      ENDIF
      IF (KSTEP>ignoreStep) THEN
          IF (mod(KSTEP-ignoreStep,NResetStep)==1.AND.KINC==1) then !每个循环第一步的第一个增量步 重置预设值 并传递前一循环应变分量
              UVAR(3:8)=UVAR(18:23)
              UVAR(9:14)=UVAR(18:23)!为了补充Inc1~Inc0的应变范围，迫不得已多申请了7个变量……
              UVAR(1)=UVAR(24)
              UVAR(2)=UVAR(24)
              UVAR(16)=-huge(UVAR(16))
              FLAGforDO=1
              UVAR(33)=-huge(UVAR(16))
          ENDIF
          call getvrm('EP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          SIGNCORRECT=SIGN(1.0D0,ARRAY(1)+ARRAY(2)+ARRAY(3))
          call getvrm('E',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          DEQP1=(array(1)-array(2))**2.0D0+(array(2)-array(3))**2.0D0+
     1    (array(3)-array(1))**2.0D0
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
          call getvrm('TEMP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          CTEMP=ARRAY(1)
          IF (CTEMP>UVAR(33)) THEN
              UVAR(33)=CTEMP
          ENDIF
!Calculate the range between bigger and littler
      IF (FLAGforDo.EQ.1) THEN
          FLAGforDo=0
          DE(1:6)=UVAR(3:8)-UVAR(9:14)
          DEQP1=(DE(1)-DE(2))**2.0D0+(DE(2)-DE(3))**2.0D0+
     1    (DE(3)-DE(1))**2.0D0
          DEQP2=DE(4)**2.0D0+DE(5)**2.0D0+DE(6)**2.0D0          !假设E4~6都是工程剪应变γ
          UVAR(15)=0.47140452079103173D0*SQRT(DEQP1+DEQP2*1.5)  !Vstar is 0.5
          IF (UVAR(15).GT.UVAR(16)) THEN
              UVAR(16)=UVAR(15)
          ENDIF
      ENDIF
      ELSE !如果是ignoreStep，仅保存应变分量和当量，不计算U15 U16
          call getvrm('EP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          SIGNCORRECT=SIGN(1.0D0,ARRAY(1)+ARRAY(2)+ARRAY(3))
          call getvrm('E',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          DEQP1=(array(1)-array(2))**2D0+(array(2)-array(3))**2.0D0+
     1    (array(3)-array(1))**2D0
          DEQP2=array(4)**2.0D0+array(5)**2.0D0+array(6)**2.0D0 !假设E4~6都是工程剪应变γ
          DEQ=0.47140452079103173D0*SQRT(DEQP1+DEQP2*1.5D0)*SIGNCORRECT !Vstar is 0.5
          UVAR(18:23)=array(1:6)
          UVAR(24)=DEQ
      ENDIF
C**********FatigueStrainRangeCommand Bottom**********      
C**********CreepDamageCommand Top**********
      IF (KSTEP==1.AND.KINC==1) THEN
          call getvrm('SINV',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          CMISES=ARRAY(1)
          call getvrm('TEMP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          CTEMP=ARRAY(1)
          call getvrm('SP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          CSPXY=(/ARRAY(1),ARRAY(2),ARRAY(3)/)
          UVAR(30)=(CSPXY(1)+CSPXY(2)+CSPXY(3))/
     1    SQRT(CSPXY(1)**2+CSPXY(2)**2+CSPXY(3)**2)
C          call getvrm('S',array,jarray,flgray,jrcd,jmac,jmatyp,
C     1 matlayo,laccfla)
C          CSXY=(/ARRAY(1),ARRAY(2),ARRAY(3)/)
          REPSQ={{REP_SQ}}(CMISES,CSPXY)    !前端传入选用的等效应力方法(ASME,RCC),REP_SQ是函数,REPSQ是等效应力
          UVAR(17)=CTEMP
          UVAR(25)=REPSQ
      ELSEIF (KSTEP>1.OR.KINC>1) THEN !当不是STEP 1 INC 0/1的时候
          call getvrm('SINV',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          CMISES=ARRAY(1)
          call getvrm('TEMP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          CTEMP=ARRAY(1)
          call getvrm('SP',array,jarray,flgray,jrcd,jmac,jmatyp,
     1    matlayo,laccfla)
          CSPXY=(/ARRAY(1),ARRAY(2),ARRAY(3)/)
          UVAR(30)=(CSPXY(1)+CSPXY(2)+CSPXY(3))/
     1    SQRT(CSPXY(1)**2+CSPXY(2)**2+CSPXY(3)**2)
          REPSQ={{REP_SQ}}(CMISES,CSPXY)
          IF (CTEMP>MinCreepTemp) THEN                          !计算蠕变损伤
              TempGAP=abs(CTEMP-UVAR(17))/TempThreshold         !两步温度差决定的插值间隔
              StressGAP=abs(REPSQ-UVAR(25))/StressThreshold     !两步蠕变损伤用等效应力的差决定的插值间隔
              GAP=MAX(int(TempGAP),int(StressGAP),1)
              DURTIME=DTIME/GAP
              do i=1,GAP
                  DTEMP=CTEMP*(i-0.5D0)/GAP+UVAR(17)*(1.0D0-(i-0.5D0)/GAP)
                  REPDS=REPSQ*(i-0.5D0)/GAP+UVAR(25)*(1.0D0-(i-0.5D0)/GAP)
                  UVAR(27)=UVAR(27)+{{CRDAMAGE}}(SF,REPDS,CTEMP,DURTIME)
C 经测试SF09和SF67这些位置直接使用常数会导致并行求解错误                  
              enddo
          ENDIF
          UVAR(17)=CTEMP
          UVAR(25)=REPSQ    !在求完损伤后,赋值UVAR(25)为当前增量步下的蠕变损伤用等效应力
      ENDIF
C**********CreepDamageCommand Bottom**********
C**********FatigueDamage Top**********
      IF (Time(1)==Time1forFatigue.and. !循环最后一分析步时间恰好为指定值时（用来判断最后一增量步）
     1  mod(KSTEP-ignoreStep,NResetStep)==0.and. !每个循环最后一分析步
     2  UVAR(16)>0) THEN !应变范围不为0
          UVAR(31)=UVAR(31)+
     1    1.0D0/loglog_interpolate_{{user_FatigueLife_method}}(UVAR(16),UVAR(33))
      ENDIF
C**********FatigueDamage Bottom**********   !使用交互作用图确定蠕变-疲劳损伤
      UVAR(32)=is_above(UVAR(31),UVAR(27))
      RETURN
      END

C***********REP_SQ***********
!REP_SQ_start
!REP_SQ_end

C***********Creep Damage Calculate***********
!Creep_Damage_start
!Creep_Damage_end


C***********Fatigue Life Calculate***********
!Fatigue_life_model_start
!Fatigue_life_model_end


C***********Creep-Fatigue Enevlope***********
!Creep_Fatigue_Enevlope_start
!Creep_Fatigue_Enevlope_end