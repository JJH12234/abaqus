      SUBROUTINE CreepNBPN_q0(DECRA,EC,QTILD,TIME,DTIME,
     1 LEXIMP,NBPNpara,timescale)
      DIMENSION DECRA(5),TIME(3),EC(2),NBPNpara(7)
      INTEGER,intent(in) :: LEXIMP
      REAL*8,intent(in) :: DTIME,QTILD,EC,TIME,timescale
      REAL*8 DECRA
      REAL*8 dep0,q0,q1,n1,Q,QQ,A,N,M,XM,P1,P2,P3,P4,NBPNpara
      dep0=NBPNpara(2)
      q0=NBPNpara(3)
      N=NBPNpara(4)
      M=NBPNpara(5)
      q1=NBPNpara(6)
      n1=NBPNpara(7)
      XM=1.0D0/(M+1.0D0)
      A=dep0**(M+1.0D0) !非原有A
      A1=dep0 !非原有A1
      Q=QTILD/q0
      QQ=QTILD/q1
      P1=A*Q**N*XM
      P3=(TIME(3)*timescale)**(M+1.0D0)-(TIME(3)*timescale-DTIME*timescale)**(M+1.0D0)
      DECRA(1)=P1*P3+A1*QQ**N1*DTIME*timescale
      IF (LEXIMP.EQ.1) THEN
          DECRA(5)=A*N*XM*Q**(N-1)/q0*P3+A1*N1*QQ**(N1-1)/q1*DTIME*timescale
      ENDIF
      RETURN
      END