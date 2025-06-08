C****************************************************
C use Langer model to calculate Nf
C****************************************************
      function loglog_interpolate_Langer(U16,maxTEMP)
        implicit none
        ! Input parameters
        real*8 :: para({{para}})
        real*8 :: temps({{temps}})
        real*8, intent(in) :: U16,maxTEMP            ! Given strain range and max temperature
!Fatigue_life_langermodel_start
        REAL*8, DIMENSION(4, 3) :: Lpara= RESHAPE(
     1 (/704D0, 0.45809D0, -0.62904D0, 0.00327D0,
     2 871D0, 0.84659D0, -0.74498D0, 0.00249D0,
     3 950D0, 2.70614D0, -0.85561D0, 0.00153D0/),
     4 (/4,3/))
!Fatigue_life_langermodel_end
        ! Output result
        real*8 :: loglog_interpolate_Langer
        ! Local variables
        integer :: idx, rows, cols, idx_low, idx_high
        real*8 :: t1, t2, ratio, Nf1, Nf2
        real*8 :: i
        rows = size(Lpara,1)
        cols = size(Lpara,2)
        if (maxTEMP <= Lpara(1,1)) then
            para = Lpara(2:rows,1)
            loglog_interpolate_Langer = ((U16-para(3))/para(1))
     1        **(1/para(2))
            return
        elseif (maxTEMP >= Lpara(1,cols)) then
            para = Lpara(2:rows,cols)
            loglog_interpolate_Langer = ((U16-para(3))/para(1))
     1        **(1/para(2))
            return
        else
            idx_low = 0
            idx_high = 0
            temps = Lpara(1,:)
            do i = 1, cols
              if (temps(i) <= maxTEMP .and. maxTEMP <= temps(i+1)) then
                idx_low = i
                idx_high = i+1
                exit
              end if
            end do
            t1 = temps(idx_low)
            t2 = temps(idx_high)
            Nf1 = ((U16-Lpara(4,idx_low))/Lpara(2,idx_low))
     1        **(1/Lpara(3,idx_low))
            Nf2 = ((U16-Lpara(4,idx_high))/Lpara(2,idx_high))
     1        **(1/Lpara(3,idx_high))
            ratio = (maxTEMP - t1)/(t2 - t1)
            loglog_interpolate_Langer = Nf1 + ratio*(Nf2-Nf1)
            return
        end if
        end function loglog_interpolate_Langer