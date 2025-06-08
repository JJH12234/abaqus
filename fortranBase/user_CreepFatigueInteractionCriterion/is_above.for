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
        elseif ({{k0}}D0*Fatigue>Creep) then
            k={{k2}}D0        !-9.0d0
            b={{b2}}D0        !1.0d0
        !elseif (Creep > 0.1d0) then
        elseif (Creep>={{k0}}D0*Fatigue) then
            k={{k1}}D0        !-1.0d0/9.0d0
            b={{b1}}D0        !1.0d0/9.0d0
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