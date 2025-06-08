C****************************************************
C use CreepNB_TH_q0(Time Hardening) as Creep CS
C****************************************************
      SUBROUTINE CreepNB_TH_q0(DECRA,EC,QTILD,TIME,DTIME,
     1 LEXIMP,NBpara,timescale)
      DIMENSION DECRA(5),TIME(3),EC(2),NBpara(7)
      INTEGER,intent(in) :: LEXIMP
      REAL*8,intent(in) :: DTIME,QTILD,EC,TIME,timescale
      REAL*8 DECRA
      REAL*8 dep0,q0,Q,A,N,M,XM,P1,P2,P3,P4,NBpara
      dep0=NBpara(2)
      q0=NBpara(3)
      N=NBpara(4)
      M=NBpara(5)
      XM=1.0D0/(M+1.0D0)
      A=dep0**(M+1.0D0) !非原有A
      Q=QTILD/q0
      P1=A*Q**N*XM
C     Time Hardening
      P3=(TIME(3)*timescale)**(M+1.0D0)-(TIME(3)*timescale-DTIME*timescale)**(M+1.0D0)
      DECRA(1)=P1*P3
      IF (LEXIMP.EQ.1) THEN
      DECRA(5)=A*N*XM*Q**(N-1.0D0)/q0*P3
      ENDIF
      RETURN
      END