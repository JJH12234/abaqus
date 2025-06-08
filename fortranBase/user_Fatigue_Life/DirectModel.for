C****************************************************
C use Direct(chazhi) model to calculate Nf
C****************************************************
      function loglog_interpolate_Direct(U16,maxTEMP)
        implicit none
        ! Input parameters
        real*8 :: strain_range({{strain_range_num}})   ! 单调递减的应变范围
        real*8 :: life_data({{strain_range_num}})      ! 单调递增的life
        real*8 :: temps({{temps_num}})
        real*8, intent(in) :: U16,maxTEMP              ! 给定的应变范围和最高温度
!Fatigue_life_Directmodel_start
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