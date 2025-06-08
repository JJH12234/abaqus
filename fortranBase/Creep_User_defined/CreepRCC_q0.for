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

