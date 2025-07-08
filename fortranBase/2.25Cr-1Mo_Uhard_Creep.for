  
************************************************************************
**: <ABAQUS user subroutine, Uhard>
**:  developed by Rou 2023.08.31
**:  the constiutive model of plastic part for 2.25Cr-1Mo alloy in ASME 2023
************************************************************************
**:
**: <Uhard heading>
**:
      SUBROUTINE UHARD(SYIELD,HARD,EQPLAS,EQPLASRT,TIME,DTIME,TEMP,
     &     DTEMP,NOEL,NPT,LAYER,KSPT,KSTEP,KINC,CMNAME,NSTATV,
     &     STATEV,NUMFIELDV,PREDEF,DPRED,NUMPROPS,PROPS)
C
      INCLUDE 'ABA_PARAM.INC'
	  implicit none
C
      CHARACTER*80 CMNAME

c      user coding to define SYIELD,HARD(1),HARD(2),HARD(3)
      
	  integer NOEL,npt, LAYER,KSPT,KSTEP,KINC,NSTATV,NUMFIELDV,NUMPROPS
	  
	  real(8) syield,eqplas,temp,EQPLASRT,dtime,DTEMP
	  real(8) x_sy,x_B,x_C
	  real(8) temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8
	  real(8) temp9,temp10,temp11
      real(8) x_sy1,x_sy2,x_sy3,x_sy4,x_sy5,x_sy6,x_sy7,x_sy8
	  real(8) x_sy9,x_sy10,x_sy11
	  real(8) x_B1,x_B2,x_B3,x_B4,x_B5,x_B6,x_B7,x_B8,x_B9,x_B10,x_B11
	  real(8) x_C1,x_C2,x_C3,x_C4,x_C5,x_C6,x_C7,x_C8,x_C9,x_C10,x_C11	
      real(8) HARD(3),STATEV(NSTATV),time(2)
      real(8) PREDEF(NUMFIELDV),DPRED(2),PROPS(NUMPROPS)


**********************************************************************
**********************************************************************有效列的长度 
**********************************************************************

****************************************************    
**:   Clarify the unit in ASME
****************************************************
**:   temperature: ℃; 
**:   stres: MPa;
**:   time: h;
**:   strain: mm/mm
**:   strain refers to engineering strain;   stress refers to engineering stress


**: Temperaure points: 
      temp1 = 371.0
	  temp2 = 399.0
	  temp3 = 427.0
	  temp4 = 454.0
	  temp5 = 482.0
	  temp6 = 510.0
      temp7 = 538.0
      temp8 = 566.0
	  temp9 = 593.0
	  temp10 = 621.0
	  temp11 = 649.0
	  
**:  value of sy at temperature points
      x_sy1 = 210.96
	  x_sy2 = 193.42
	  x_sy3 = 199.53
	  x_sy4 = 194.18
	  x_sy5 = 183.83
	  x_sy6 = 180.78
      x_sy7 = 183.29
      x_sy8 = 173.76
	  x_sy9 = 169.4
	  x_sy10 = 153.3
	  x_sy11 = 133.88
 
	  
**:  value of B at temperature points
      x_B1 = 576.81
	  x_B2 = 541.04
	  x_B3 = 477.98
	  x_B4 = 487.86
	  x_B5 = 601.37
	  x_B6 = 418.51
      x_B7 = 389.23
      x_B8 = 317.16
	  x_B9 = 300.79
	  x_B10 = 277.6
	  x_B11 = 239.85
  	
**:  value of C at temperature points
      x_C1 = -25.59
	  x_C2 = -32.09
	  x_C3 = -39.9
	  x_C4 = -35.38
	  x_C5 = -26.45
	  x_C6 = -45.35
      x_C7 = -40.45
      x_C8 = -60.25
	  x_C9 = -52.16
	  x_C10 = -58.78
	  x_C11 = -65.17

****************************************************    
**:   Input value of parameters
****************************************************     
      
**:	Constant sy: 
      if (temp>=temp1 .and. temp<temp2) then
	     x_sy = x_sy1 + (temp-temp1)*(x_sy2 - x_sy1)/((temp2-temp1))
	  else if (temp>=temp2 .and. temp<temp3) then   
		 x_sy = x_sy2 + (temp-temp2)*(x_sy3 - x_sy2)/((temp3-temp2))
	  else if (temp>=temp3 .and. temp<temp4) then   
		 x_sy = x_sy3 + (temp-temp3)*(x_sy4 - x_sy3)/((temp4-temp3))
	  else if (temp>=temp4 .and. temp<temp5) then   
		 x_sy = x_sy4 + (temp-temp4)*(x_sy5 - x_sy4)/((temp5-temp4))
	  else if (temp>=temp5 .and. temp<temp6) then   
		 x_sy = x_sy5 + (temp-temp5)*(x_sy6 - x_sy5)/((temp6-temp5))
	  else if (temp>=temp6 .and. temp<temp7) then   
		 x_sy = x_sy6 + (temp-temp6)*(x_sy7 - x_sy6)/((temp7-temp6))		 
	  else if (temp>=temp7 .and. temp<temp8) then   
		 x_sy = x_sy7 + (temp-temp7)*(x_sy8 - x_sy7)/((temp8-temp7))		 
	  else if (temp>=temp8 .and. temp<temp9) then   
		 x_sy = x_sy8 + (temp-temp8)*(x_sy9 - x_sy8)/((temp9-temp8))		 		 
	  else if (temp>=temp9 .and. temp<temp10) then   
		 x_sy = x_sy9 + (temp-temp9)*(x_sy10 - x_sy9)/((temp10-temp9))		 
	  else if (temp>=temp10 .and. temp<temp11) then   
		 x_sy = x_sy10 + (temp-temp10)*(x_sy11 - x_sy10)/((temp11-temp10))
	  end if 


**:	Constant B: 
      if (temp>=temp1 .and. temp<temp2) then
	     x_B = x_B1 + (temp-temp1)*(x_B2 - x_B1)/((temp2-temp1))
	  else if (temp>=temp2 .and. temp<temp3) then   
		 x_B = x_B2 + (temp-temp2)*(x_B3 - x_B2)/((temp3-temp2))
	  else if (temp>=temp3 .and. temp<temp4) then   
		 x_B = x_B3 + (temp-temp3)*(x_B4 - x_B3)/((temp4-temp3))
	  else if (temp>=temp4 .and. temp<temp5) then   
		 x_B = x_B4 + (temp-temp4)*(x_B5 - x_B4)/((temp5-temp4))
	  else if (temp>=temp5 .and. temp<temp6) then   
		 x_B = x_B5 + (temp-temp5)*(x_B6 - x_B5)/((temp6-temp5))
	  else if (temp>=temp6 .and. temp<temp7) then   
		 x_B = x_B6 + (temp-temp6)*(x_B7 - x_B6)/((temp7-temp6))		 
	  else if (temp>=temp7 .and. temp<temp8) then   
		 x_B = x_B7 + (temp-temp7)*(x_B8 - x_B7)/((temp8-temp7))		 
	  else if (temp>=temp8 .and. temp<temp9) then   
		 x_B = x_B8 + (temp-temp8)*(x_B9 - x_B8)/((temp9-temp8))		 		 
	  else if (temp>=temp9 .and. temp<temp10) then   
		 x_B = x_B9 + (temp-temp9)*(x_B10 - x_B9)/((temp10-temp9))		 
	  else if (temp>=temp10 .and. temp<temp11) then   
		 x_B = x_B10 + (temp-temp10)*(x_B11 - x_B10)/((temp11-temp10))
	  end if 

**:	Constant C: 
      if (temp>=temp1 .and. temp<temp2) then
	     x_C = x_C1 + (temp-temp1)*(x_C2 - x_C1)/((temp2-temp1))
	  else if (temp>=temp2 .and. temp<temp3) then   
		 x_C = x_C2 + (temp-temp2)*(x_C3 - x_C2)/((temp3-temp2))
	  else if (temp>=temp3 .and. temp<temp4) then   
		 x_C = x_C3 + (temp-temp3)*(x_C4 - x_C3)/((temp4-temp3))
	  else if (temp>=temp4 .and. temp<temp5) then   
		 x_C = x_C4 + (temp-temp4)*(x_C5 - x_C4)/((temp5-temp4))
	  else if (temp>=temp5 .and. temp<temp6) then   
		 x_C = x_C5 + (temp-temp5)*(x_C6 - x_C5)/((temp6-temp5))
	  else if (temp>=temp6 .and. temp<temp7) then   
		 x_C = x_C6 + (temp-temp6)*(x_C7 - x_C6)/((temp7-temp6))		 
	  else if (temp>=temp7 .and. temp<temp8) then   
		 x_C = x_C7 + (temp-temp7)*(x_C8 - x_C7)/((temp8-temp7))		 
	  else if (temp>=temp8 .and. temp<temp9) then   
		 x_C = x_C8 + (temp-temp8)*(x_C9 - x_C8)/((temp9-temp8))		 		 
	  else if (temp>=temp9 .and. temp<temp10) then   
		 x_C = x_C9 + (temp-temp9)*(x_C10 - x_C9)/((temp10-temp9))		 
	  else if (temp>=temp10 .and. temp<temp11) then   
		 x_C = x_C10 + (temp-temp10)*(x_C11 - x_C10)/((temp11-temp10))
	  end if 

 
	  if (eqplas < 1.0d-6 .and. eqplas > 0.0d0) then 
	     eqplas = 1.0d-6
	  end if 

**************************************************************
**:   Expression of stress as the function of plastic strain 
************************************************************** 
	  
	  syield = (x_sy - x_B)*exp(x_C*eqplas) + x_B
	  
**:  HARD(1) is derivate for strain, HARD(2) is derivative for strain rate 
**:  HARD(3) is derivative for temperature	 
 
	  hard(1)= x_C*(x_sy - x_B)*exp(x_C*eqplas)
	  hard(2)= 0.0d0
      hard(3)= 0.0d0	  	 	 
		 
      RETURN
      END
	  




****************************************************************************
**: <ABAQUS user subroutine, Creep>
**:  developed by Rou 2023.08.31
**:  the constiutive model of creep part for 2.25Cr-1Mo alloy in ASME 2023
****************************************************************************
**:
**: <Creep heading>
**:	  

		SUBROUTINE CREEP(DECRA,DESWA,STATEV,SERD,EC,ESW,P,QTILD,
     &       TEMP,DTEMP,PREDEF,DPRED,TIME,DTIME,CMNAME,LEXIMP,LEND,
     &       COORDS,NSTATV,NOEL,NPT,LAYER,KSPT,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
	  implicit none
C
      CHARACTER*80 CMNAME
C

      integer NSTATV,NOEL,NPT,LAYER,KSPT,KSTEP,KINC,LEXIMP,LEND
	  real(8) DECRA(5),DESWA(5),STATEV(NSTATV),SERD,EC(2),ESW(2)
	  real(8) p,QTILD,TEMP,DTEMP,PREDEF(3),DPRED(3),TIME(3)
	  real(8) DTIME,COORDS(3)
	 
	  real(8) x_U
	  real(8) temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8
	  real(8) x_U1,x_U2,x_U3,x_U4,x_U5,x_U6,x_U7,x_U8
	  
	  real(8) x_C1,x_C2,x_P1,x_P2,x_rate1,x_rate2  
	  real(8) x_d1,x_d2,x_q1,x_q2,x_e1,x_e2	  
	  
	  real(8) x_t1,aa,bb
	  real(8) stn1_pie
	  real(8) xx,yy,x_tc,x_t_pie
	  real(8) dC1dStss,dP1dStss,dRate1dStss
	  real(8) dC2dStss,dP2dStss,dRate2dStss	  
	  real(8) dt1adStss,dt1bdStss,dxt1dStss
	  real(8) dstn1piedStss,dxxdStss,dyydStss,dxtcdStss,dxtpiedStss
	  real(8) stn1,stn2,stn3,stss
	  real(8) dPC1,dPC2,CP1,CP2
**********************************************************************
**********************************************************************有效列的长度 
**********************************************************************


****************************************************    
**:   Clarify the unit in ASME
****************************************************
**:   temperature: ℃; 
**:   stres: MPa;
**:   time: h;
**:   strain: mm/mm
**:   strain refers to engineering strain;   stress refers to engineering stress

	  
**: Temperaure points: 
      temp1 = 371.0
	  temp2 = 400.0
	  temp3 = 450.0
	  temp4 = 500.0
	  temp5 = 550.0
	  temp6 = 600.0
      temp7 = 621.0
      temp8 = 649.0
	  	  
	  
**:  value of sy at temperature points
      x_U1 = 471.0
	  x_U2 = 468.0
	  x_U3 = 452.0
	  x_U4 = 418.0
	  x_U5 = 364.0
	  x_U6 = 284.0
      x_U7 = 300.0
      x_U8 = 270.0
	  
**:	Constant U: 
      if (temp>=temp1 .and. temp<temp2) then
	     x_U = x_U1 + (temp-temp1)*(x_U2 - x_U1)/((temp2-temp1))
	  else if (temp>=temp2 .and. temp<temp3) then   
		 x_U = x_U2 + (temp-temp2)*(x_U3 - x_U2)/((temp3-temp2))
	  else if (temp>=temp3 .and. temp<temp4) then   
		 x_U = x_U3 + (temp-temp3)*(x_U4 - x_U3)/((temp4-temp3))
	  else if (temp>=temp4 .and. temp<temp5) then   
		 x_U = x_U4 + (temp-temp4)*(x_U5 - x_U4)/((temp5-temp4))
	  else if (temp>=temp5 .and. temp<temp6) then   
		 x_U = x_U5 + (temp-temp5)*(x_U6 - x_U5)/((temp6-temp5))
	  else if (temp>=temp6 .and. temp<temp7) then   
		 x_U = x_U6 + (temp-temp6)*(x_U7 - x_U6)/((temp7-temp6))		 
	  else if (temp>=temp7 .and. temp<temp8) then   
		 x_U = x_U7 + (temp-temp7)*(x_U8 - x_U7)/((temp8-temp7))		 
	  end if 




      if(QTILD < 1.0D-6) then
	      QTILD = 1.0D-6
      end if


	 
**:	Expression of C1,P1,rate1: 	 
	  x_d1 = 1.0328 + 168680.0/x_U/(temp+273.15) - 0.023772*x_U + 
     &	      0.0079141*x_U*log10(QTILD)
	  x_C1 = 10.0**x_d1
	  
	  x_q1 = 7.6026 + 3.3396*log10(QTILD) - 12323.0/(temp+273.15)
	  x_P1 = 10.0**x_q1
	  
	  x_e1 = 6.7475 + 0.01142*QTILD + 987.72/x_U*log10(QTILD) - 
     &  	  13494.0/(temp+273.15)
	  x_rate1 = 10.0**x_e1

**:	Expression of C2,P2,rate2: 	
	  x_d2 = -0.051086 + 140730.0/x_U/(temp+273.15) - 0.01*x_U + 
     &	     0.0037345*x_U*log10(QTILD)
	  x_C2 = 10.0**x_d2
	  
	  x_q2 = 8.1242 + 0.0179678*QTILD + 404.63/x_U*log10(QTILD) - 
     &	     11659.0/(temp+273.15)
	  x_P2 = 10.0**x_q2
	  
	  x_e2 = 11.498-8.2226*x_U/(temp+273.15)-20448.0/(temp+273.15)
     &   	  + 5862.4/(temp+273.15)*log10(QTILD)
	  x_rate2 = 10.0**x_e2


**:	Expression of t1,aa,bb
      aa = -13.528 + 6.5196*x_U/(temp+273.15) + 23349.0/(temp+273.15)
     &   	  - 5693.8/(temp+273.15)*log10(QTILD)
	  
	  bb = -11.098 - 4.0951*QTILD/x_U + 11965.0/(temp+273.15)
	  
	  if (temp<=454.0) then
	     x_t1 = 10.0**aa
	  else if (temp>454.0 .and. temp<=510.0) then
	     x_t1 = 10.0**aa + (10.0**bb - 10.0**aa)*(temp - 454.0)/56.0
	  else 
	     x_t1 = 10.0**bb
	  end if 
	  
**:	Expression of stn1_pie
      stn1_pie = x_C1*x_P1*x_t1/(1.0+x_P1/x_t1) + x_rate1*x_t1
	  	  	  
			  
**:	Expression of x_tc
***** note: xx and yy are intermediate variables 
      xx = 4.0*stn1_pie*x_rate2*x_P2 + (x_rate2 + 
     &   	  (x_C2 - stn1_pie)*x_P2)**2.0
	  
	  yy = -1*x_rate2 - x_C2*x_P2 + stn1_pie*x_P2 + xx**0.5
      
	  x_tc = yy/(2.0*x_rate2*x_P2)	  
	  

**:	Expression of x_t_pie
      x_t_pie = time(3) - (x_t1 - x_tc)


c      write(6,*)"yy = ", yy 
c      write(6,*)"x_rate2 = ", x_rate2  	  
c      write(6,*)"x_P2 = ", x_P2
c      write(6,*)"QTILD= ", QTILD	  

	  
*****************************************************************************************************************************	  
***        Above equations are used for creep strain rate 
***        The following equations are used for deviation of creep strain rate to stress 	  
*****************************************************************************************************************************	
  	   	  
**:	Expression of deviation of  C1,P1,rate1 to stress 
      dC1dStss = 10.0**x_d1*(0.0079141*x_U/QTILD)
	  dP1dStss = 10.0**x_q1*(3.3396/QTILD)
	  dRate1dStss = 10.0**x_e1*log(10.0)*
     &       	  (0.01142+987.72/x_U/(QTILD*log(10.0)))
	  
      dC2dStss = 10.0**x_d2*(0.0037345*x_U/QTILD)	  
	  dP2dStss = 10.0**x_q2*log(10.0)*(0.0179678+404.63/
     &          	  x_U/(QTILD*log(10.0)))
	  dRate2dStss = 10.0**x_e2*5862.4/(temp+273.15)/QTILD

**:   add by Lin, shotcut for P*dCdS+C*dPdS, C*P
      dPC1=x_P1*dC1dStss + x_C1*dP1dStss
      dPC2=x_P2*dC2dStss + x_C2*dP2dStss
      CP1=x_C1*x_P1
      CP2=x_C2*x_P2
************************************************************************
**:	Expression of deviation of  x_t_pie to stress
************************************************************************

**:	Expression of deviation of  x_t1 to stress
      dt1adStss = 10.0**aa*(-5693.8/(temp+273.15)/QTILD)
	  dt1bdStss = 10.0**bb*log(10.0)*(-4.0951/x_U)
	  
	  if (temp<=454.0) then
	     dxt1dStss = dt1adStss
	  else if (temp>454.0 .and. temp<=510.0) then
	     dxt1dStss=dt1adStss+(dt1bdStss-dt1adStss)*(temp-454.0)/56.0
	  else 
	     dxt1dStss = dt1bdStss
	  end if 



**:	Expression of deviation of  stn1_pie to stress

      dstn1piedStss = ((x_P1*x_t1*dC1dStss + x_C1*x_t1*dP1dStss + 
     &      	  x_C1*x_P1*dxt1dStss)*(1.0+x_P1*x_t1) - 
     &      	 x_C1*x_P1*x_t1*(x_P1*dxt1dStss + x_t1*dP1dStss))/
     &     	 (1+x_P1*x_t1)**2.0 + x_rate1*dxt1dStss + x_t1*dRate1dStss
	  
	  
**:	Expression of deviation of xx to stress	  
      dxxdStss = 4.0*x_rate2*x_P2*dstn1piedStss + 
     &	  4.0*stn1_pie*x_P2*dRate2dStss + 4.0*stn1_pie*x_rate2*dP2dStss 
     &    + 2.0*(x_rate2 + (x_C2 - stn1_pie)*x_P2)*(dRate2dStss + 
     &  	 (dC2dStss-dstn1piedStss)*x_P2+(x_C2-stn1_pie)*dP2dStss)
 

**:	Expression of deviation of yy to stress	 
      dyydStss = -1.0*dRate2dStss - x_C2*dP2dStss - x_P2*dC2dStss +
     &    stn1_pie*dP2dStss+x_P2*dstn1piedStss+0.5*xx**(-0.5)*dxxdStss
	  
	  
**:	Expression of deviation of x_tc to stress	 
      dxtcdStss = (x_rate2*x_P2*dyydStss - yy*(x_P2*dRate2dStss 
     &      	  + x_rate2*dP2dStss))/2.0/(x_rate2*x_P2)**2.0
	  
	  
**:	Final expression of deviation of x_t_pie to stress	 
      dxtpiedStss = dxtcdStss - dxt1dStss 



*****************************************************************************************************************************	  
***        The following equations are used for calculating the creep strain	  
*****************************************************************************************************************************	
      stn1 = x_C1*x_P1*time(3)/(1.0+x_P1*time(3))+x_rate1*time(3)
	  stn2 = x_C2*x_P2*time(3)/(1.0+x_P2*time(3))+x_rate2*time(3)
	  stn3 = x_C2*x_P2*x_t_pie/(1.0+x_P2*x_t_pie)+x_rate2*x_t_pie
	  

c      write(6,*)"stn1 = ", stn1
c      write(6,*)"stn2 = ", stn2	  
c      write(6,*)"stn3 = ", stn3
c      write(6,*)"time(3) = ", time(3)	  
c      write(6,*)"temp = ", temp
	  
	  
	  	   
******************************************************************:
**: creep strain rate:  (1)


** set the value of stress, to avoid Nan problem at the beginning of simulation



      if (temp >= 372.0) then  
	   
	     if (stn2 < stn1) then
               DECRA(1)=(CP2*Time(3)/(1.+x_P2*Time(3))
     &          -CP2*(Time(3)-DTIME)/(1.+x_P2*(Time(3)-DTIME))
     &          +x_rate2*DTIME)/100.
!		    DECRA(1)=(x_C1*x_P1/(1.0+x_P1*time(3))
!     &			**2.0+x_rate1)/100.0*dtime
		 else if (time(3) < x_t1) then
               DECRA(1)=(CP1*Time(3)/(1.+x_P1*Time(3))
     &          -CP1*(Time(3)-DTIME)/(1.+x_P1*(Time(3)-DTIME))
     &          +x_rate1*DTIME)/100.
!		    DECRA(1)=(x_C2*x_P2/(1.0+x_P2*time(3))
!     &   			**2.0+x_rate2)/100.0*dtime
!为了结构件收敛性建议这里补充time3-Dtime在一阶段但是time3在二阶段的情况               
		 else 
               DECRA(1)=(CP2*x_t_pie/(1.+x_P2*x_t_pie)
     &          -CP2*(x_t_pie-DTIME)/(1.+x_P2*(x_t_pie-DTIME))
     &          +x_rate2*DTIME)/100.
!		    DECRA(1)=(x_C2*x_P2/(1.0+x_P2*x_t_pie)
!     &			**2.0+x_rate2)/100.0*dtime	   
!为了结构件收敛性建议这里补充time3-Dtime在二阶段但是time3在三阶段的情况               
         end if 

	
	
******************************************************************:	  
**: deviation of creep strain rate to stress: DECRA(5)
	   
      if (LEXIMP.eq.1) then
	     if (stn2 < stn1) then
               DECRA(5) =((dPC2*Time(3)*(1.+x_P2*Time(3))
     &          -CP2*Time(3)**2.*dP2dStss)/((1.+x_P2*time(3))**2.)
     &          -
     &          (dPC2*(Time(3)-DTIME)*(1.+x_P2*(Time(3)-DTIME))-
     &          CP2*(Time(3)-DTIME)**2.*dP2dStss)
     &          /((1.+x_P2*(Time(3)-DTIME))**2.)
     &          +
     &          dRate2dStss*DTIME)/100.

!		    DECRA(5) = (((x_P1*dC1dStss + x_C1*dP1dStss)*
!     &    	(1.0+x_P1*time(3))**2.0-x_C1*x_P1*2.0*(1.0+x_P1*time(3))
!     &      *time(3)*dP1dStss)/(1.0+x_P1*time(3))**4.0+dRate1dStss)
!     &   	 /100.0*dtime 
           else if (time(3) < x_t1) then
               DECRA(5) =((dPC1*Time(3)*(1.+x_P1*Time(3))
     &          -CP1*Time(3)**2.*dP1dStss)/((1.+x_P1*time(3))**2.)
     &          -
     &          (dPC1*(Time(3)-DTIME)*(1.+x_P1*(Time(3)-DTIME))-
     &          CP1*(Time(3)-DTIME)**2.*dP1dStss)
     &          /((1.+x_P1*(Time(3)-DTIME))**2.)
     &          +
     &          dRate1dStss*DTIME)/100.
!		    DECRA(5) = (((x_P2*dC2dStss + x_C2*dP2dStss)*
!     &		(1.0+x_P2*time(3))**2.0-x_C2*x_P2*2.0*(1.0+x_P2*time(3))
!     &      *time(3)*dP2dStss)/(1.0+x_P2*time(3))**4.0+dRate2dStss)
!     &   	 /100.0*dtime 
           else 
             DECRA(5)=(((dPC2*x_t_pie+CP2*dxtpiedStss)*(1.+x_P2*x_t_pie)
     &          -CP2*x_t_pie*(dP2dStss*x_t_pie+x_P2*dxtpiedStss))
     &          /((1.+x_P2*x_t_pie)**2.)
     &          -
     &         (dPC2*(x_t_pie-DTime)+CP2*dxtpiedStss)*
     &          (1.+x_P2*(x_t_pie-DTime))-
     &          CP2*(x_t_pie-DTime)*
     &         (dP2dStss*(x_t_pie-Dtime)+x_P2*dxtpiedStss)
     &          /((1.+x_P2*(x_t_pie-DTime))**2.)
     &          +
     &         dRate2dStss*DTIME)/100.
!		    DECRA(5) = (((x_P2*dC2dStss + x_C2*dP2dStss)*
!     &      (1.0+x_P2*x_t_pie)**2.0-x_C2*x_P2*2.0*(1.0+x_P2*x_t_pie)
!     &      *(x_t_pie*dP2dStss+x_P2*dxtpiedStss))/
!     &   	 (1+x_P2*x_t_pie)**4.0+dRate2dStss)/100.0*dtime  
           end if 
      endif
	  
  	  else
	   DECRA(1) = 0.0d0
	   DECRA(5) = 0.0d0 
	  
	  end if 	  
    
	  RETURN
      END
	  
**********************************************************************
**********************************************************************有效列的长度 
**********************************************************************	  