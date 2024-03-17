! iRainSnowHydro.f90 
!
! FUNCTIONS:
! iRainSnowHydro - Entry point of console application.
! main program by Ke Zhang, Yuning Luo, Yuhao Wang, Sheng Wang 2023-8-15

!****************************************************************************
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program iRainSnowHydro
    Implicit none
    !DEC$OBJCOMMENT LIB:'dll1.lib'
                 interface
                 subroutine Dll1(Nx, Ny, GridPobs, GridTc, GridTf, GridDs, ProbS, DensityS, Ps, Pr, SCF1, TotalPre_rain, TotalPre_snow, KLWL, PSO, SNM, Ksn, SCF, SnowCoef)
                 !DEC$ ATTRIBUTES DLLEXPORT::Dll1
                  Integer, Intent(Inout) :: Nx, Ny
                  Real, allocatable, Intent(Inout) :: GridPobs(:,:), GridTc(:,:), GridTf(:,:), GridDs(:,:)
                  Real, Intent(Inout) :: ProbS, DensityS, Ps, Pr, SCF1, TotalPre_rain, TotalPre_snow
                  Real, Intent(Inout) :: KLWL, PSO, SNM, Ksn, SCF, SnowCoef
                  end subroutine Dll1
                 end interface   
    
    Character SSName*100
    Integer DTS,HTS,TSteps , LongFDTime,DRMGCNo  
    Real, allocatable:: GGE(:) , GEE(:),AvgP(:),DRMET(:),  DRMPO(:),DRMIca(:) , DRMIch(:)
    Real NfS,NfC,SumDRMIca,Cellsize,CSAlpha,DEMPrecision, IW
    Real, allocatable:: QSim(:,:) ,SimQ(:) ,Dis(:),Pcum(:,:),Icum(:,:), Pnet(:)
    Real, allocatable:: QSimi(:,:) ,QSimg(:,:),QSimch(:,:),QSims(:,:) 
    Real, allocatable:: gridriout(:,:,:) 
    Integer i1,j1,m,Kp,Grfc  
    Real Sumdis,Sump,GLAI,Scmax, Cp, Cvd  
    Integer,allocatable::RFC(:,:) 
    Integer  SaP, pp, qq, p, q, new_ii, new_jj
    Real GW , GS , GWM , GSM , ThitaS , ThitaF , ThitaW  , Thita, Tc_tmp
    Real   SumKgKi  , KgKi , Kg , Ki , KKg , KKi 
    Real R  , Rs  , Ri  , Rg  , GQs  , GQi  , GQg , Gqch  
    Real DWD,SS0 , SSf , DWU,ZUpper , ZLower , ZDeeper 
    Real GWUM , GWU , GWLM , GWL , GWDM , GWD 
    Real, allocatable::GridWU0(:,:)  , GridWL0(:,:)  , GridWD0(:)  
    Real GE , GEU , GEL, ged, Dp
    
    
    Integer JYear, JMonth, JDay, nd, kk1, kk2 
    Real GR, GRs, GRi, GRg
    Real, allocatable::GRi_temp(:,:)
    Real MC1, MC2, MC3, MDt 
    
    Real, allocatable:: InflowQs(:,:), OutflowQs(:,:), InflowQi(:,:) , OutflowQi(:,:)
    Real, allocatable::InflowQg(:,:), OutflowQg(:,:) 
    Real, allocatable::SumQs(:), SumQi(:), SumQg(:),SumQch(:),ET0out(:) 
    Real SumOQ, SumSQ, OPeak, SPeak, SumEE 
    Integer OPeakTime,SPeakTime
    Real SumPre, DArea
    Real ORunoff, SRunoff, ONC , SNC, AvgOQ
    Real Qoutch, Qouts, Qouti, Qoutg, CCS1
    Integer,allocatable::UPRow(:) , UPCol(:) ,NoSM(:),NoWM(:)
    Character,allocatable:: UPName(:)*100
    Real, allocatable::UPSimQ(:) , UPQSim(:,:), UPSimH(:),UPHSim(:)  
    Real, allocatable::InflowQch(:,:),OutflowQch(:,:)
    Real,allocatable::EObs(:,:),Pobs(:,:),QObs(:),Pob(:)
    
    Real Eob,Qob,DT1
    Integer i,j,k,ntime,ii,jj,kk,ntime1,DT,TimeStepCmp,kkk      
    Integer IY,IM,ID,IH,ITI,ITI1 
    Integer NoBasin,IYB,IYE,NoEvents,Ncalsort,IMB,IME,IDB,IDE,NoStation
    Integer,allocatable:: IY1(:),IM1(:),ID1(:),IH1(:),IY2(:),IM2(:),ID2(:),IH2(:)
    Character STCD*100,ctemp*100
    Real,allocatable:: SortingOrder(:),SortingRow(:),SortingCol(:)
    Real STSWC(20),STFC(20),STWP(20)
    Real  LAI(20,12), MaxLAI(20),CTopH(20),LumPara(30)
    Real OC , ROC , CSBeta , DeeperC 
    Real KLWL !added by LWL
    Real KEpC, AlUpper , AlLower , AlDeeper , CCS 
    Real CCg , CCci, MKch , MXch, MKs 
    Real MXs , MKi, MXi , MKg , MXg , ki_tmp
    Integer Nx , Ny,LagTime,LagTime1
    Real XllCorner , YllCorner , DDem , Nodata 
    Real Dy , Dx , Garea, PI  
    Real,allocatable::RasterData(:,:,:) 
    Real,allocatable:: E(:,:) , WaterArea(:,:) , RiverPoint(:,:) , FlowDirection(:,:) , DRMOrderNo(:,:) , DRMWM(:,:) , DRMSM(:,:) 
    Real,allocatable:: DRMMNfC(:,:), DRMAlpha(:,:), DRMMNfS(:,:)  , DRMBmax(:,:) , DRMSSlope(:,:) , DRMCSlope(:,:) , DRMfc(:,:) 
    Real,allocatable:: SType030(:,:) , SType30100(:,:) , TopIndex(:,:) , LCover(:,:), DRMKg(:,:), DRMKi(:,:) , HumousT(:,:) , ThickoVZ(:,:) , GridWM(:,:), GridSM(:,:)
    Real,allocatable:: BasBulk(:,:)  
    Real,allocatable::GridQi(:,:) ,GridQg(:,:) , GridQs(:,:)
    Real,allocatable::GridRi(:,:) 
    Real,allocatable::GridW(:,:)  , GridS(:,:)  , GridFLC(:,:)  
    Real,allocatable::GridWU(:,:) , GridWL(:,:) , GridWD(:,:)  
    Real,allocatable::GridHIni(:,:,:)
    Real Ct, Ek,ETKcb, ETKcbmax, ETKcbmin,Flc 
    Real,allocatable::GridQSim(:,:,:), &
                      GridQSimi(:,:,:), &
                      GridQSimg(:,:,:), &
                      GridQSims(:,:,:), &
                      GridQSimch(:,:,:), &
                      GridPObs(:,:), &
                      GridEObs(:,:), &
                      GridSlope(:,:), GridDEM(:,:), TotalQ_tmp(:,:)   
    Real,allocatable::TotalQig_tmp(:,:),TotalQsch_tmp(:,:)               
    Real,allocatable::TotalQobs(:,:), TotalQout(:,:), GridQSimTemp(:,:), TotalQ(:,:), TotalLumPara(:,:), lonlat(:,:), lonlat_outlet(:,:), Param_a(:,:), Param_I(:,:)  
    Real,allocatable::GridTc(:,:)
    Real,allocatable::GridTf(:,:)
    Real,allocatable::GridDs(:,:)
    Real,allocatable::initial_GridQSim(:,:),SumQ(:,:),StaQSim(:,:)
    Real,allocatable::GridQSimTempig(:,:),GridQSimTempsch(:,:),StaQSim2(:,:)
    Real,allocatable::initial_GridQSimi(:,:),initial_GridQSimg(:,:),initial_GridQSimch(:,:),initial_GridQSims(:,:),GridQSimiTemp(:,:),GridQSimgTemp(:,:), GridQSimsTemp(:,:),GridQSimchTemp(:,:)!20231220
   
    Integer,allocatable::PSCol(:), PSRow(:), NextNoIJ(:,:)
    Real StrLat, StrLon
    Integer ObsDataLen
    
    Integer ENo 
    Real Div, Cg , Ci , Pe , TotalQtmp, rainfact
    Real,allocatable::OutLetQ(:)  , PMean(:)  
    Real,allocatable::GridQ(:),QCal(:),PPe(:),TotalPre(:,:)
    Integer jNow  
    
    Real,allocatable::Qout(:)
    Real VBAL,OFV,RC
    Integer Inopt,Inwrite,Nwarmday
    Real Smean, Wmean,SCF,SnowCoef, FreeWaterCoef,FreeWaterCoef1
    Integer Niter,Nsm, count, Inrunmethod
    character DXU*1,Cdate*10
    character Inputdate*10,Inputdate1*8, InputBasin*2  
    real MaxDs, ProbS, DensityS, Ps, Pr, min_dem, max_dem, min_slope, max_slope, StaQ, SCF1,TotalPre_rain,TotalPre_snow, KgKi_tmp
    real PSO, SNM, Ksn 
    integer save_state,load_state
    
    character path_ec*300, path_sta*300, rain_path*300, temp_path*300
    integer rain_source, row_data, col_data, row_basin, col_basin, ii_tmp, jj_tmp
    real x_data, y_data, cs_data, x_basin, y_basin, cs_basin
    Real,allocatable::GridPdata(:,:), GridTdata(:,:)
  
    
!=======Read catchment====================
    open(11,file='CatID.dat')     
    read(11,*) STCD !catchment name
    read(11,*) ctemp
    read(11,*) NoBasin
    read(11,*) ctemp
    read(11,*) NoStation
    read(11,*) ctemp
    read(11,*) path_ec
    read(11,*) path_sta
    close(11)
    print *,'Catchment name:',trim(STCD)

    
    open(11,file='Configure.dat')
    read(11,*) ctemp
    read(11,*)  IYB,IMB,IDB
    read(11,*) ctemp
    read(11,*) IYE,IME,IDE
    read(11,*) ctemp
    read(11,*)  Nwarmday
    read(11,*) ctemp
    read(11,*) Nsm ! Initial soil moisture
    read(11,*) ctemp
    read(11,*) SCF
    read(11,*) ctemp
    read(11,*) SnowCoef
    read(11,*) ctemp
    read(11,*) FreeWaterCoef
    read(11,*) ctemp
    read(11,*) FreeWaterCoef1
    read(11,*) ctemp
    read(11,*) save_state
    read(11,*) ctemp
    read(11,*) load_state
    read(11,*) ctemp
    read(11,*) rain_source
    close(11)
    
    print *,'Calibration period:',IYB,IMB,IDB,'--',IYE,IME,IDE
    TimeStepCmp=1
    DT=24
    Inrunmethod=2 ! Muskingum

!=================parameters optimization:1, only one parameter set simulation:0  
   Inopt=0
   Niter=10000
!================write the soil state:1,else 0
!   Inwrite=0

!====================Data length calculation
   open(12,file='.\data\Datetime.txt')   
   
   read(12,*,end=99) ctemp
   i=1
  do while(.true.)
  read(12,*,end=99) IY,IM,ID,IH 
        ITI=(IY-IYB)*(IY-IYE)
        
        If (ITI .lt. 0)  then
            i=i+1
        elseif ((ITI .eq. 0)  .and. (IYB .ne. IYE))then
            if (IY .eq. IYB) then
               if (IM .gt. IMB) then
                   i=i+1
               elseif (IM .eq. IMB) then
                    if (ID .ge. IDB) then
                        i=i+1
                    endif
               endif
            elseif (IY .eq. IYE) then
                if (IM .lt. IME) then
                    i=i+1
               elseif (IM .eq. IME) then
                    if (ID .le. IDE) then
                        i=i+1
                    endif
               endif    
            endif
        elseif ((ITI .eq. 0)  .and. (IYB .eq. IYE))then
            ITI1=(IM-IMB)*(IM-IME)
            if (ITI1 .lt. 0) then
                i=i+1
            elseif ((ITI1 .eq. 0) .and. (IMB .ne. IME)) then
                if ((IM .eq. IMB) .and. (ID .ge. IDB) ) then
                    i=i+1
                elseif  ((IM .eq. IME) .and. (ID .le. IDE) ) then
                    i=i+1
                endif
            elseif ((ITI1 .eq. 0) .and. (IMB .eq. IME)) then    
                if ((ID .ge. IDB) .and. (ID .le. IDE)) then
                    i=i+1
                endif
            endif
        endif
     enddo
99      continue 
        close(12)
        ntime=i-1 ! ntime = hourly time
        ntime1=ntime/DT ! ntime1 = daily time
        print *,'Time steps of simulation',ntime1
        
!====================define year, month, day, hour=============== 
        allocate(IY1(ntime),IM1(ntime),ID1(ntime),IH1(ntime))
        allocate(IY2(ntime1),IM2(ntime1),ID2(ntime1),IH2(ntime1))
        open(13,file='.\data\Datetime.txt')  
         read(13,*,end=98) ctemp
        i=1
         do while(.true.)
        read(13,*,end=98) IY,IM,ID,IH
        ITI=(IY-IYB)*(IY-IYE)
        If (ITI .lt. 0)  then
              IY1(i)=IY
              IM1(i)=IM
              ID1(i)=ID
              IH1(i)=IH
            i=i+1
        elseif ((ITI .eq. 0)  .and. (IYB .ne. IYE))then
            if (IY .eq. IYB) then
               if (IM .gt. IMB) then
                  IY1(i)=IY
                  IM1(i)=IM
                  ID1(i)=ID
                  IH1(i)=IH                  
                  i=i+1
               elseif (IM .eq. IMB) then
                    if (ID .ge. IDB) then
                        IY1(i)=IY
                        IM1(i)=IM
                        ID1(i)=ID
                        IH1(i)=IH                       
                        i=i+1
                    endif
               endif
            elseif (IY .eq. IYE) then
                if (IM .lt. IME) then
              IY1(i)=IY
              IM1(i)=IM
              ID1(i)=ID
              IH1(i)=IH                    
                  i=i+1
               elseif (IM .eq. IME) then
                    if (ID .le. IDE)then
               IY1(i)=IY
              IM1(i)=IM
              ID1(i)=ID
              IH1(i)=IH                       
                        i=i+1
                    endif
                    
               endif    
            endif
        elseif ((ITI .eq. 0)  .and. (IYB .eq. IYE))then
            ITI1=(IM-IMB)*(IM-IME)
            if (ITI1 .lt. 0) then
              IY1(i)=IY
              IM1(i)=IM
              ID1(i)=ID
              IH1(i)=IH                
                i=i+1
            elseif ((ITI1 .eq. 0) .and. (IMB .ne. IME)) then
                if ((IM .eq. IMB) .and. (ID .ge. IDB) ) then
              IY1(i)=IY
              IM1(i)=IM
              ID1(i)=ID
              IH1(i)=IH                    
                    i=i+1
                elseif  ((IM .eq. IME) .and. (ID .le. IDE) ) then
              IY1(i)=IY
              IM1(i)=IM
              ID1(i)=ID
              IH1(i)=IH                    
                    i=i+1
                endif
            elseif ((ITI1 .eq. 0) .and. (IMB .eq. IME)) then    
                if ((ID .ge. IDB) .and. (ID .le. IDE))  then
              IY1(i)=IY
              IM1(i)=IM
              ID1(i)=ID
              IH1(i)=IH                    
                    i=i+1
                endif
            endif
        endif
 
        
         enddo   
98      continue
        close(13)
        
        do i=1,ntime1
            IY2(i)=IY1(1+(i-1)*DT)
            IM2(i)=IM1(1+(i-1)*DT)
            ID2(i)=ID1(1+(i-1)*DT)
            IH2(i)=IH1(1+(i-1)*DT)
        enddo

!=====================Read discharge=====================================================================
        allocate(TotalQobs(ntime1,NoStation), & ! Observed discharge
                TotalQ(100000,4+NoStation), & ! 4 = yymmddhh, useless in Yunnan project
                TotalQout(ntime1,NoStation))! Daily simulated discharge
        allocate(QObs(ntime1))
        TotalQout=0.0
        open(14,file='.\data\QH_' // trim(STCD) // '.txt')
        read(14,*,end=1234) ctemp
        do i=1,100000
            read(14,*,end=1234) (TotalQ(i,j),j=1,4+NoStation)
        enddo
1234    close(14)
        
        do ii=1,ntime1! ii = daily time
            do jj=1,100000! jj = upper limit of daily time
                if (TotalQ(jj,1) .eq. IY2(ii)) then
                    if (TotalQ(jj,2) .eq. IM2(ii)) then
                        if (TotalQ(jj,3) .eq. ID2(ii)) then
                            if (TotalQ(jj,4) .eq. IH2(ii)) then
                                if (TimeStepCmp .eq. 1) then
                                    do kk=1,NoStation
                                      TotalQobs(ii,kk)=TotalQ(jj,kk+4)
                                    enddo
                                elseif (TimeStepCmp .eq. 0) then! TimeStepCmp = 1, following elseif is useless
                                    do kk=1,NoStation
                                        TotalQtmp=0.
                                        do i=jj,DT+jj-1
                                            TotalQtmp=TotalQtmp+TotalQ(i,kk+4)! kk = station No
                                        enddo
                                        TotalQobs(ii,kk)=TotalQtmp/DT! Calculate daily average discharge 
                                    enddo 
                                endif
                                exit
                            endif
                        endif
                    endif
                endif
            enddo
        enddo                    
        
    97     continue
        close(14)
       print *,'Total length of observed discharge:', ntime1

!===================Read soil type,here starts from 1
          open(16,file='.\data\Soiltype.dat')
          read(16,*,end=95) ctemp
          i=1
           do while(.true.)
              read(16,*,end=95)  j, &
                                 ctemp,STSWC(i), &
                                 STFC(i),STWP(i)
              i=i+1
          enddo
95        close(16)
          
!==================Read land cover information,here starts from 1
          open(17,file='.\data\Landcover.dat')
          read(17,*,end=94) ctemp
          i=1
            do while(.true.)
              read(17,*,end=94) k, &
                                ctemp,(LAI(i,j),j=1,12), MaxLAI(i), CTopH(i)
              i=i+1
          enddo
94        close(17)
              
 ! read DEM infomation
   open(1,file='.\data\ASC_file\' //trim(STCD)// '_DEM.asc')
   read(1,*,end=92) ctemp, Ny ! Cols
   read(1,*,end=92) ctemp, Nx ! Rows
   read(1,*,end=92) ctemp, XllCorner ! Left bottom lon
   read(1,*,end=92) ctemp, YllCorner ! Left bottom lan
   read(1,*,end=92) ctemp, DDem ! Cellsize
   read(1,*,end=92) ctemp, Nodata
     
 !=============Convert degree to meter
    
    PI = 3.141592654
    Dy = 6370.997 * DDem * PI / 180 * 1000
    Dx = 6370.997 * Cos(YllCorner * PI / 180.0) * DDem * PI / 180.0 * 1000.
    Cellsize = Int(DDem / (30. / 3600.) * 1000.)
  
!===============Size for each grid(km**2)
    Garea = Dx * Dy/1000000.0         

!==========read Grid infomation
   open(2,file='.\data\ASC_file\' // trim(STCD)// '_RiverChannel.asc')
   do i=1,6
       read(2,*,end=92) ctemp
   enddo
   
      open(3,file='.\data\ASC_file\' // trim(STCD)// '_TensionWaterCapacity.asc')
   do i=1,6
       read(3,*,end=92) ctemp
   enddo
   
      open(4,file='.\data\ASC_file\' // trim(STCD)// '_FreedomWaterCapacity.asc')
   do i=1,6
       read(4,*,end=92) ctemp
   enddo
   
      open(5,file='.\data\ASC_file\' // trim(STCD)// '_0-30cmSoilType.asc')
   do i=1,6
       read(5,*,end=92) ctemp
   enddo
   
      open(6,file='.\data\ASC_file\' // trim(STCD)// '_30-100cmSoilType.asc')
   do i=1,6
       read(6,*,end=92) ctemp
   enddo
   
      open(7,file='.\data\ASC_file\' // trim(STCD)// '_HumusSoilDepth.asc')
   do i=1,6
       read(7,*,end=92) ctemp
   enddo
   
      open(8,file='.\data\ASC_file\' // trim(STCD)// '_VegetationType.asc')
   do i=1,6
       read(8,*,end=92) ctemp
   enddo
   
   open(9,file='.\data\ASC_file\' // trim(STCD)// '_FlowAccumulationArea.asc')
   do i=1,6
       read(9,*,end=92) ctemp
   enddo
   
         open(10,file='.\data\ASC_file\' // trim(STCD)// '_RunoffDistributionRatio.asc')
   do i=1,6
       read(10,*,end=92) ctemp
   enddo
    
         open(11,file='.\data\ASC_file\' // trim(STCD)// '_GridFlowDirection.asc')
   do i=1,6
       read(11,*,end=92) ctemp
   enddo
    
         open(12,file='.\data\ASC_file\' // trim(STCD)// '_VadoseZoneDepth.asc')
   do i=1,6
       read(12,*,end=92) ctemp
   enddo
   
   print *,'read gridded variables'
   
   allocate(RasterData(Nx,Ny,12))   
   do ii = 1 , Nx
           read(1,*,end=92) (RasterData(ii, jj, 1),jj=1,Ny)
            read(2,*,end=92) (RasterData(ii, jj, 2),jj=1,Ny)
            read(3,*,end=92)  (RasterData(ii, jj, 3),jj=1,Ny)
            read(4,*,end=92) (RasterData(ii, jj, 4),jj=1,Ny)
            read(5,*,end=92) (RasterData(ii, jj, 5),jj=1,Ny)
            read(6,*,end=92) (RasterData(ii, jj, 6),jj=1,Ny)
            read(7,*,end=92) (RasterData(ii, jj, 7),jj=1,Ny)
            read(8,*,end=92) (RasterData(ii, jj, 8),jj=1,Ny)
            read(9,*,end=92) (RasterData(ii, jj, 9),jj=1,Ny)
            read(10,*,end=92) (RasterData(ii, jj, 10),jj=1,Ny)
             read(11,*,end=92) (RasterData(ii, jj, 11),jj=1,Ny)
             read(12,*,end=92) (RasterData(ii, jj, 12),jj=1,Ny)
   enddo
92 continue
   close(1)
   close(2)
   close(3)
   close(4)
   close(5)
   close(6)
   close(7)
   close(8)
   close(9)
   close(10)
   close(11)
   close(12)
   close(13)

   
    do ii=1,Nx
           do jj=1,Ny
               RasterData(ii,jj,4)=RasterData(ii,jj,4)*FreeWaterCoef ! Free water capacity
               RasterData(ii,jj,3)=RasterData(ii,jj,3)*FreeWaterCoef1 ! Tension water capacity
               RasterData(ii,jj,7)=RasterData(ii,jj,7)*FreeWaterCoef
               RasterData(ii,jj,12)=RasterData(ii,jj,12)*FreeWaterCoef1
               !RasterData(ii,jj,7)=RasterData(ii,jj,7)*FreeWaterCoef1 ! Old: Humus soil depth, but why FreeWaterCoef1(TensionWaterCoef)?
               !RasterData(ii,jj,12)=RasterData(ii,jj,12)*FreeWaterCoef ! Old:
           enddo
        enddo 
   
   
   allocate(GridDs(Nx,Ny)) 
   if (load_state .eq. 0) then
       open(14,file='.\data\ASC_file\' // trim(STCD)// '_SnowDepth.asc')
       do i=1,6
           read(14,*,end=921) ctemp
       enddo
       
       do ii=1,Nx
           read(14,*,end=921) (GridDs(ii,jj),jj=1,Ny)
       enddo
   else
       open(14,file='.\data\Initial_state\' // trim(STCD)// '_SnowDepth.asc')
       do i=1,6
           read(14,*,end=921) ctemp
       enddo
       
       do ii=1,Nx
           read(14,*,end=921) (GridDs(ii,jj),jj=1,Ny)
       enddo
   endif
   
   
921 close(14)

   !=====================compute initial depth of snow=========Ds=0.873+0.907DEM-0.256slope, this function is normalized, shoule be de-normalized
   !this part was done and as a parameter which is needed to read
   
   
!=========================read parameters of calculating pet=================
   allocate(Param_a(Nx,Ny),Param_I(Nx,Ny))
   open(1030,file='.\data\Param_a.asc')
   do i=1,6
       read(1030,*,end=10301) ctemp
   enddo
   
   open(1031,file='.\data\Param_I.asc')
   do i=1,6
       read(1031,*,end=10301) ctemp
   enddo
   
   do ii = 1 , Nx
           read(1030,*,end=10301) (Param_a(ii, jj),jj=1,Ny)
            read(1031,*,end=10301) (Param_I(ii, jj),jj=1,Ny)
   enddo
10301 close(1030)
      close(1031)

  
!==============define initial GridQsim============= 
    Allocate(GridQSim(Nx,  Ny, ntime1)) 
    do ii=1,Nx
        do jj=1,Ny
            do kk=1,ntime1
                GridQSim(ii,jj,kk)=0
            enddo
        enddo
    enddo
 !==============define initial GridQsimi=============
    Allocate(GridQSimi(Nx,  Ny, ntime1)) 
    do ii=1,Nx
        do jj=1,Ny
            do kk=1,ntime1
                GridQSimi(ii,jj,kk)=0
            enddo
        enddo
    enddo
    
!==============define initial GridQsimg=============Yuning Luo  20231220
    Allocate(GridQSimg(Nx,  Ny, ntime1)) ! Gridded simulated discharge
    do ii=1,Nx
        do jj=1,Ny
            do kk=1,ntime1
                GridQSimg(ii,jj,kk)=0
            enddo
        enddo
    enddo
!==============define initial GridQsims=============
    Allocate(GridQSims(Nx,  Ny, ntime1)) 
    do ii=1,Nx
        do jj=1,Ny
            do kk=1,ntime1
                GridQSims(ii,jj,kk)=0
            enddo
        enddo
    enddo  
!==============define initial GridQsimch=============
    Allocate(GridQSimch(Nx,  Ny, ntime1)) 
    do ii=1,Nx
        do jj=1,Ny
            do kk=1,ntime1
                GridQSimch(ii,jj,kk)=0
            enddo
        enddo
    enddo

!===================read outlet info of sub basins================
    allocate(lonlat(NoStation,3), lonlat_outlet(NoStation,3))
    open(100,file='.\Data\Qobs_Station_of_Subbasin.txt')
    read(100,*,end=1001) ctemp
    do i=1,NoStation
        read(100,*,end=1001) (lonlat(i,j),j=1,3)
    enddo
     
1001 close(100)
     
!==============check whether the outlet point is located in the river==================
     do i=1,NoStation
         if (lonlat(i,3) .ne. Nodata) then
             ii=Nx+1-ceiling((lonlat(i,3)-yllcorner)/DDem) ! Outlet point rows
             jj=ceiling((lonlat(i,2)-xllcorner)/DDem)      ! Outlet point cols
             print *,RasterData(ii,jj,2),ii,jj
             print *,RasterData(ii,jj,9)
         endif
     enddo
     pause
     
        
!=============read lumpped parameters of all sub-basin===============
     print *,'Read lumped parameters'
    !allocate(TotalLumPara(100,22))
    allocate(TotalLumPara(100,23)) !added 0807
           open(18,file='.\data\Lumpara_' // trim(STCD) // '.txt')
            read(18,*,end=93) ctemp
            i=1
            do while(.true.)
                read(18,*,end=93) (TotalLumPara(i,j),j=1,23 ) !added 0807
                !read(18,*,end=93) (TotalLumPara(i,j),j=1,22 )
                i=i+1
            enddo
93          close(18)

!===================allocate initial grid data=======================
    !info of rain data
    x_data=85.00
    y_data=40.00
    cs_data=0.05
    row_data=401
    col_data=401
    
    !info of basin
    x_basin=XllCorner ! Left bottom
    y_basin=YllCorner+Nx*DDem ! Left top
    cs_basin=DDem
    row_basin=Nx
    col_basin=Ny
    
    allocate(initial_GridQSim(Nx,Ny),initial_GridQSimi(Nx,Ny),initial_GridQSimg(Nx,Ny),initial_GridQSims(Nx,Ny),initial_GridQSimch(Nx,Ny)) 
    Allocate(GridQi(Nx, Ny), GridQg(Nx, Ny), GridQs(Nx, Ny))
    Allocate (GRi_temp(Nx,  Ny))
    Allocate(GridPObs(Nx,  Ny),GridTc(Nx,Ny), GridTf(Nx,Ny))
    Allocate(GridEObs(Nx,Ny), GridPdata(row_data,col_data), GridTdata(row_data,col_data))
    Allocate(GridW(Nx, Ny), GridS(Nx, Ny), GridFLC(Nx, Ny))
    Allocate(GridWU(Nx,  Ny), GridWL( Nx,Ny), GridWD(Nx,Ny))
    Allocate(GridHIni(Nx, Ny, 4))
    allocate(E(Nx, Ny), WaterArea(Nx, Ny), RiverPoint(Nx, Ny), FlowDirection(Nx, Ny), DRMOrderNo(Nx, Ny), DRMWM(Nx, Ny), DRMSM(Nx, Ny))  
    allocate(DRMMNfC(Nx, Ny), DRMAlpha(Nx, Ny), DRMMNfS(Nx, Ny), NextNoIJ(Nx, Ny), DRMBmax(Nx, Ny), DRMSSlope(Nx, Ny), DRMCSlope(Nx, Ny), DRMfc(Nx, Ny)) ! Runoff distribution ratio
    allocate(SType030(Nx, Ny), &! 0-30 soil type
            SType30100(Nx, Ny), &! 30-100 soil type
            TopIndex(Nx, Ny), &! TI
            LCover(Nx, Ny), DRMKg(Nx, Ny), DRMKi(Nx, Ny), HumousT(Nx, Ny), ThickoVZ(Nx, Ny), GridWM(Nx, Ny), GridSM(Nx, Ny))
    allocate(BasBulk(Nx, Ny),TotalPre(ntime1,3)) ! No use
    allocate (gridriout(Nx, Ny,TSteps))

    
       
            
!===================do loop of all sub-basin====================
    do 2048 kk=1,NoBasin
        InputBasin='00'
        if (kk .lt. 10) then
            WRITE(InputBasin(2:2),'(I1)') kk
        else
            WRITE(InputBasin(1:2),'(I2)') kk
        endif
        
        do i=1,ntime1
            QObs(i)=TotalQobs(i,kk)
        enddo

!====================== Read grid calculating order
        open(15,file='.\data\CalSort_'// InputBasin// '.txt')
        i=1
            read(15,*,end=961) ctemp
        do while(.true.)
         read(15,*,end=961)  j 
         i=i+1
        enddo
961       close(15)
        Ncalsort=i-1 ! Raster total number

        allocate(SortingOrder(Ncalsort),SortingRow(Ncalsort),SortingCol(Ncalsort))
        open(15,file='.\data\CalSort_'// InputBasin// '.txt')
        i=1
        read(15,*,end=96) ctemp
        do while(.true.)
         read(15,*,end=96) j,SortingOrder(i),SortingRow(i),SortingCol(i)
         i=i+1
         enddo
96       close(15)   

!==================Read lumped parameters 
         do j=1,22    
             LumPara(j)=TotalLumPara(1,j+1)
         enddo
            print *,'Lumped model parameters:'
            !print *,(LumPara(i),i=1,21)
            print *,(LumPara(i),i=1,22) !added 0807
    OC = LumPara(1)
    ROC = LumPara(2)
    KEpC = LumPara(3)
    DeeperC = LumPara(4)
    AlUpper = LumPara(5)
    AlLower = LumPara(6)
    CCg = LumPara(7)
    CCci = LumPara(8)
 !------------------------------------
    CCS = LumPara(9)
    LagTime1 = LumPara(12)! LagTime1 = HM
    CCS1 = LumPara(11)
    LagTime = LumPara(10)! LagTime = D
 !--------------------------------
     MKch = LumPara(13)
    MKs = LumPara(14)
    MKi = LumPara(15)
    MKg = LumPara(16)
    MXch = LumPara(17)
    MXs = LumPara(18)
    MXi = LumPara(19)
    MXg = LumPara(20)
    Ki_tmp=LumPara(21)
    KLWL= LumPara(22) 
    AlDeeper = 1. - AlUpper - AlLower

 !====================Calculation of KIKG according to soil properties

    DRMGCNo = 0.
    SumKgKi = 0.
      do j = 1 , Ncalsort
            ii = SortingRow(j)
            jj = SortingCol(j)
            
            E(ii, jj) = RasterData(ii, jj, 1)         
            RiverPoint(ii, jj) = RasterData(ii, jj, 2)  
            DRMWM(ii, jj) = RasterData(ii, jj, 3)
            DRMSM(ii, jj) = RasterData(ii, jj, 4)      
            SType030(ii, jj) = RasterData(ii, jj, 5)+1   
            SType30100(ii, jj) = RasterData(ii, jj, 6)+1  
            HumousT(ii, jj) = RasterData(ii, jj, 7)
            LCover(ii, jj) = RasterData(ii, jj, 8)+1   
            DRMfc(ii, jj) = RasterData(ii, jj, 10)
            FlowDirection(ii, jj) = RasterData(ii, jj, 11)
            ThickoVZ(ii, jj) = RasterData(ii, jj, 12)
            If (E(ii, jj) .ne. Nodata) Then
                DRMGCNo = DRMGCNo + 1
                NextNoIJ(ii, jj) = DRMGCNo
                If (HumousT(ii, jj) .le. 300) Then
                    ThitaS = STSWC(SType030(ii, jj))
                    ThitaF = STFC(SType030(ii, jj))
                    ThitaW = STWP(SType030(ii, jj))
                Else
                    ThitaS = STSWC(SType030(ii, jj)) * (300. / HumousT(ii, jj)) + STSWC(SType30100(ii, jj)) * (1. - 300. / HumousT(ii, jj))
                    ThitaF = STFC(SType030(ii, jj)) * (300. / HumousT(ii, jj)) + STFC(SType30100(ii, jj)) * (1. - 300. / HumousT(ii, jj))
                    ThitaW = STWP(SType030(ii, jj)) * (300. / HumousT(ii, jj)) + STWP(SType30100(ii, jj)) * (1. - 300. / HumousT(ii, jj))
                Endif
                DRMKi(ii, jj) = Ki_tmp
                DRMKg(ii, jj) = 0.7-Ki_tmp
                if (isnan(DRMKi(ii,jj))) then
                    DRMKi(ii, jj)=Ki_tmp
                endif
                if (isnan(DRMKg(ii,jj))) then
                    DRMKg(ii, jj)=0.7-Ki_tmp
                endif
                KgKi_tmp=(ThitaF / ThitaS) ** OC
                if (isnan(KgKi_tmp)) then
                    KgKi_tmp=0.7
                endif
                SumKgKi = SumKgKi + KgKi_tmp
            Endif
                 
      enddo
    KgKi = SumKgKi / float(DRMGCNo)
    
!=========================== soil capacity 

    JMonth = IM1(1)
 
    ETKcbmin = 0.175

 !=================Estimate Flc for each grid for the first time period
if (load_state .eq. 0) then
    do 91 j = 1, DRMGCNo
        ii = SortingRow(j)
        jj = SortingCol(j)
        ETKcb = 1.07 * (1 - Exp(-0.84 * LAI(LCover(ii, jj), JMonth)))
        ETKcbmax = 1.07 * (1 - Exp(-0.84 * MaxLAI(LCover(ii, jj))))
        If (ETKcb - ETKcbmin .lt. 0) Then
            Flc = 0.
        Else
            Flc = ((ETKcb - ETKcbmin) / (ETKcbmax + 0.05 - ETKcbmin)) ** (1. + 0.5 * CTopH(LCover(ii, jj)))
        Endif
        GridFLC(ii, jj) = Flc
        
        Dp = DRMfc(ii, jj)
        GWM = DRMWM(ii, jj)
        GSM = DRMSM(ii, jj)
        Kg = DRMKg(ii, jj)
        Ki = DRMKi(ii, jj)
        ZUpper = AlUpper * ThickoVZ(ii, jj)
        ZLower = AlLower * ThickoVZ(ii, jj)
        ZDeeper = AlDeeper * ThickoVZ(ii, jj)        
   
        If (ZUpper .gt. 300.) Then            
            GWUM = (STFC(SType030(ii, jj)) - STWP(SType030(ii, jj))) * 300. + (STFC(SType30100(ii, jj)) - STWP(SType30100(ii, jj))) * (ZUpper - 300.)
            GWLM = (STFC(SType30100(ii, jj)) - STWP(SType30100(ii, jj))) * ZLower
        Else
            GWUM = (STFC(SType030(ii, jj)) - STWP(SType030(ii, jj))) * ZUpper
            If (ZUpper + ZLower .gt. 300.0) Then
                GWLM = (STFC(SType030(ii, jj)) - STWP(SType030(ii, jj))) * (300. - ZUpper) + (STFC(SType30100(ii, jj)) - STWP(SType30100(ii, jj))) * (ZLower - 300. + ZUpper)
            Else
                GWLM = (STFC(SType030(ii, jj)) - STWP(SType030(ii, jj))) * ZLower
            Endif
        Endif
        
 !=======================Initial state of soil capacity
        GWDM = GWM - GWUM - GWLM
        GridWU(ii, jj) = GWUM / 10.*Nsm
        GridWL(ii, jj) = GWLM /10.*Nsm
        GridWD(ii, jj) = GWDM / 10.*Nsm
        GridW(ii, jj) = GridWU(ii, jj) + GridWL(ii, jj) + GridWD(ii, jj)
        GridS(ii, jj) = DRMSM(ii, jj) /10.*Nsm
        
!=========================Initial state of interflow and ground water
        if (QObs(1) .gt. 0) then
        GridQi(ii, jj) = QObs(1) / float(DRMGCNo) / 2.
        GridQg(ii, jj) = QObs(1) / float(DRMGCNo) / 2.
        GridQs(ii, jj) = QObs(1) / float(DRMGCNo) / 2.
        else
        GridQi(ii, jj) = 0.
        GridQg(ii, jj) = 0.
        GridQs(ii, jj) = 0.
        endif
        GridHIni(ii, jj, 1) = GridW(ii, jj)
        GridHIni(ii, jj, 2) = GridS(ii, jj)
        GridHIni(ii, jj, 3) = GridWU(ii, jj)
        GridHIni(ii, jj, 4) = GridWL(ii, jj)
91 enddo
    else 
        open(21,file='.\data\Initial_state\GridW.asc')
       do i=1,6
           read(21,*,end=211) ctemp
       enddo
       
       open(22,file='.\data\Initial_state\GridS.asc')
       do i=1,6
           read(22,*,end=211) ctemp
       enddo
       
       open(23,file='.\data\Initial_state\GridWU.asc')
       do i=1,6
           read(23,*,end=211) ctemp
       enddo
       
       open(24,file='.\data\Initial_state\GridWL.asc')
       do i=1,6
           read(24,*,end=211) ctemp
       enddo
       
       open(25,file='.\data\Initial_state\GridQi.asc')
       do i=1,6
           read(25,*,end=211) ctemp
       enddo
       
       open(26,file='.\data\Initial_state\GridQg.asc')
       do i=1,6
           read(26,*,end=211) ctemp
       enddo
       
       open(27,file='.\data\Initial_state\GridQs.asc')
       do i=1,6
           read(27,*,end=211) ctemp
       enddo
       
       do ii=1,Nx
           read(21,*,end=211) (GridHIni(ii, jj, 1) ,jj=1,Ny)
           read(22,*,end=211) (GridHIni(ii, jj, 2) ,jj=1,Ny)
           read(23,*,end=211) (GridHIni(ii, jj, 3) ,jj=1,Ny)
           read(24,*,end=211) (GridHIni(ii, jj, 4) ,jj=1,Ny)
           read(25,*,end=211) (GridQi(ii,jj),jj=1,Ny)
           read(26,*,end=211) (GridQg(ii,jj),jj=1,Ny)
           read(27,*,end=211) (GridQs(ii,jj),jj=1,Ny)
       enddo
       
211    continue
       close(21)  
       close(22) 
       close(23) 
       close(24) 
       close(25) 
       close(26) 
       close(27)
    
    endif
!    
 !==============Number of flood events

    TSteps = ntime1

   allocate(PMean(TSteps))
   
   
   !===============Qsim for the begining
    Allocate(QSim(DRMGCNo, TSteps + LagTime+1))! Daily lagtime
    if (load_state .eq. 0) then
        Do ii = 1 , DRMGCNo
            Do i = 1, LagTime +1
                if (QObs(1) .gt. 0) then
                    QSim(ii, i) = QObs(1)
                else 
                    QSim(ii, i) = 0.0
                endif
            enddo
    enddo
    else
       open(141,file='.\data\Initial_state\SimGridQ.asc')
       do i=1,6
           read(141,*,end=922) ctemp
       enddo
       
       do ii=1,Nx
           read(141,*,end=922) (initial_GridQSim(ii,jj),jj=1,Ny)
       enddo
       
       Do ii = 1 , DRMGCNo
            Do i = 1, LagTime +1
                QSim(ii, i) = initial_GridQSim(SortingRow(ii),SortingCol(jj))
            enddo
       enddo
922 close(141)        
    endif   
         
        Allocate(DRMPO( DRMGCNo), DRMET( DRMGCNo))
        Allocate(Dis(NoBasin), Pcum(Nx, Ny))
        Allocate(Icum(2, DRMGCNo), DRMIca(DRMGCNo))
        Allocate(DRMIch(DRMGCNo), SimQ(TSteps+1))
        Allocate(RFC(Nx,Ny), Pnet(DRMGCNo))
        Allocate(InflowQs(2, DRMGCNo), OutflowQs(2, DRMGCNo), InflowQi(2, DRMGCNo), OutflowQi(2, DRMGCNo))
        Allocate(InflowQg(2, DRMGCNo), OutflowQg(2,DRMGCNo), InflowQch(2, DRMGCNo), OutflowQch(2, DRMGCNo))
        Allocate(NoWM(TSteps), NoSM(TSteps))
        Allocate(GridQ(DRMGCNo))
        allocate(QCal(TSteps * DRMGCNo + TSteps))
        allocate(UPQSim(TSteps+1, DRMGCNo))       
        allocate(SumQs(DRMGCNo),SumQi(DRMGCNo),SumQg(DRMGCNo))
        allocate(SumQch(DRMGCNo))
        allocate(Qout(TSteps))

 !===================Starts for simulation  
        DRMPO=0.
        PSO=0. !defined the initial state 0808
        DRMET=0.
        Dis=0.
        Pcum=0.
        Icum=0.
        DRMIca=0.
        DRMIch=0.
        SimQ=0.
        RFC=0.
        Pnet=0.
        InflowQs=0.
        OutflowQs=0.
        InflowQi=0.
        OutflowQi=0.
        InflowQg=0.
        OutflowQg=0.
        InflowQch=0.
        OutflowQch=0.
        NoWM=0
        NoSM=0

       do ii = 1, Nx
           do jj = 1, Ny
                GridWM(ii, jj) = Nodata          
                GridSM(ii, jj) = Nodata           
!=============Initial soil station for each flood event                
                GridW(ii, jj) = GridHIni(ii, jj, 1)
                GridS(ii, jj) = GridHIni(ii, jj, 2)
                GridWU(ii, jj) = GridHIni(ii, jj, 3)
                GridWL(ii, jj) = GridHIni(ii, jj, 4)
           enddo
    enddo
    
      SumDRMIca = 0.0
        Div = 5.
        Ct = Garea / DT / 3.6 ! Convert coefficient
        Cg = CCg **(DT / 24.)
        Ci = CCci**(DT / 24.)
 !=================Simulation for each time step         
       do 84 i = 1,TSteps
            NoWM(i) = 0.0 
            NoSM(i) = 0.0           
            do j=1,22 !added 0807  
             LumPara(j)=TotalLumPara(IM2(i),j+1)
            enddo

    OC = LumPara(1)
    ROC = LumPara(2)
    KEpC = LumPara(3)
    DeeperC = LumPara(4)
    AlUpper = LumPara(5)
    AlLower = LumPara(6)
    CCg = LumPara(7)
    CCci = LumPara(8) 
 !------------------------------------
    CCS = LumPara(9)
    LagTime1 = LumPara(12) 
    CCS1 = LumPara(11)
    LagTime = LumPara(10)
 !--------------------------------
 
    MKch = LumPara(13)
    MKs = LumPara(14)
    MKi = LumPara(15)
    MKg = LumPara(16)
    MXch = LumPara(17)
    MXs = LumPara(18)
    MXi = LumPara(19)
    MXg = LumPara(20)
    Ki_tmp = LumPara(21)
    KLWL = LumPara(22)
    AlDeeper = 1. - AlUpper - AlLower
 !==================FLC calculation    
            !Read the month value 
            Jday=IM1(i)         
            If (JDay  .ne. JMonth) Then
                JMonth = JDay        !Edited by Huang
                Do  jNow = 1, DRMGCNo
                    ii = SortingRow(jNow)
                    jj = SortingCol(jNow)
                    ETKcb = 1.07 * (1. - Exp(-0.84 * LAI(LCover(ii, jj), JMonth)))   ! why JMonth,not Jday?
                    ETKcbmax = 1.07 * (1. - Exp(-0.84 * MaxLAI(LCover(ii, jj))))
                    If (ETKcb - ETKcbmin .lt.  0.) Then
                        Flc = 0.
                    Else
                        Flc = ((ETKcb - ETKcbmin) / (ETKcbmax + 0.05 - ETKcbmin)) ** (1. + 0.5 * CTopH(LCover(ii, jj)))
                    Endif
                    GridFLC(ii, jj) = Flc
               enddo
            Endif
   
 !=================Define date of input data======================
            Inputdate1='00000000'
            WRITE(Inputdate1(1:4),'(I4)') IY2(i)
            if (IM2(i) .ge. 10) then 
                WRITE(Inputdate1(5:6),'(I2)') IM2(i)
            else
                WRITE(Inputdate1(6:6),'(I1)') IM2(i)
            endif
                      
            if (ID2(i) .ge. 10) then 
                WRITE(Inputdate1(7:8),'(I2)') ID2(i)
            else
                WRITE(Inputdate1(8:8),'(I1)') ID2(i)
            endif
                      
            
            print *,'Basin-', InputBasin, ', Simulating:', Inputdate1
            
    
 !==================read grid precipitation and evapotranspiration===========
 if (rain_source .eq. 1) then
     rain_path=trim(path_ec) // '\pre\'
     temp_path=trim(path_ec) // '\tem\'
 else
     rain_path=trim(path_sta) // '\pre\'
     temp_path=trim(path_sta) // '\tem\'
 endif
 

 
            open(1024,file=trim(rain_path) // Inputdate1 // '.000')
            do ii=1,4
                read(1024,*,end=10241) ctemp
            enddo

            do ii = 1, row_data
                    read(1024,*,end=10241) (GridPdata(ii, jj),jj=1,col_data)
            enddo
10241       close(1024)
         
            if (rain_source .eq. 2) then
                do ii=1, row_data
                    GridPdata(ii,:)=GridPdata(ii,:)
                enddo
            endif
                            
            do ii=1,Nx
                ii_tmp=ceiling((y_data-(y_basin-(ii-0.5)*cs_basin))/cs_data)
                if (ii_tmp .gt. row_data) then
                    ii_tmp=row_data
                endif
                do jj=1,Ny
                    jj_tmp=ceiling((x_basin+(jj-0.5)*cs_basin-x_data)/cs_data)
                    if (jj_tmp .gt. col_data) then
                        jj_tmp=col_data
                    endif
                    GridPObs(ii,jj)=GridPdata(ii_tmp, jj_tmp)  
                enddo
            enddo

 !==================read temprature===================
            open(1026,file=trim(temp_path) // Inputdate1 // '.000')
            do ii=1,4
                read(1026,*,end=10261) ctemp
            enddo
            
            do ii = 1 , row_data
                read(1026,*,end=10261) (GridTdata(ii, jj),jj=1,col_data)
            enddo
10261       close(1026)
            
            if (rain_source .eq. 2) then! No use
                do ii=1, row_data
                    GridTdata(ii,:)=GridTdata(ii,:)
                enddo
            endif
           
            do ii=1,Nx
                ii_tmp=ceiling((y_data-(y_basin-(ii-0.5)*cs_basin))/cs_data)
                if (ii_tmp .gt. row_data) then
                    ii_tmp=row_data
                endif
                do jj=1,Ny
                    jj_tmp=ceiling((x_basin+(jj-0.5)*cs_basin-x_data)/cs_data)
                    if (jj_tmp .gt. col_data) then
                        jj_tmp=col_data
                    endif
                    GridTc(ii,jj)=GridTdata(ii_tmp,jj_tmp)
                enddo
            enddo
                       
            do ii=1,Nx
                do jj=1,Ny
                    GridTf(ii,jj)=GridTc(ii,jj)*9/5+32  !convert Celsius to Fahrenheit
                enddo
            enddo 
            
 !==================calculate pet according to temperature===================
             do ii=1,Nx
                do jj=1,Ny
                    if (GridTc(ii,jj) .le. 0) then
                        GridEObs(ii,jj)=0
                    else
                        GridEObs(ii,jj)=16*(10*GridTc(ii,jj)/Param_I(ii,jj))**Param_a(ii,jj)/30.4
                    endif
                enddo
            enddo
            
 !==================Simulation for each effective grid         
         SumQs=0.
         SumQi=0.
         SumQg=0.
         SumQch=0.

      TotalPre_rain=0.
      TotalPre_snow=0.
      Do 83 k = 1 , DRMGCNo
                ii = SortingRow(k)
                jj = SortingCol(k)           
                call Dll1(Nx, Ny, GridPobs, GridTc, GridTf, GridDs, ProbS, DensityS, Ps, Pr, SCF1, TotalPre_rain, TotalPre_snow, KLWL, PSO, SNM, Ksn, SCF, SnowCoef)
                DRMPO(NextNoIJ(ii, jj)) = Pr   
!================Vegetation   interception
                PMean(i) = PMean(i) + DRMPO(NextNoIJ(ii, jj))
                DRMET(NextNoIJ(ii, jj)) = KEpC * GridEObs(ii, jj)
                GLAI = LAI(LCover(ii, jj), JMonth)
                Scmax = 0.935 + 0.498 * GLAI - 0.00575 * GLAI ** 2
                Cp = GridFLC(ii, jj)
                Cvd = 0.046 * GLAI
                Pcum(ii, jj) = Pcum(ii, jj) + DRMPO(NextNoIJ(ii, jj))! Pcum (rows, cols, time)
                Icum(2, NextNoIJ(ii, jj)) = Cp * Scmax * (1 - Exp((-Cvd) * Pcum(ii, jj) / Scmax))
                DRMIca(NextNoIJ(ii, jj)) = Icum(2, NextNoIJ(ii, jj)) - Icum(1, NextNoIJ(ii, jj))
                Icum(1, NextNoIJ(ii, jj)) = Icum(2, NextNoIJ(ii, jj))
                              
                If (DRMET(NextNoIJ(ii, jj)) .ge. DRMIca(NextNoIJ(ii, jj))) Then
                    DRMET(NextNoIJ(ii, jj)) = DRMET(NextNoIJ(ii, jj)) - DRMIca(NextNoIJ(ii, jj))! Soil evaporation
                    DRMIca(NextNoIJ(ii, jj)) = 0.0
                    If (DRMET(NextNoIJ(ii, jj)) + DRMIca(NextNoIJ(ii, jj)) .ge. SumDRMIca) Then! (DRMET(NextNoIJ(ii, jj)) + DRMIca(NextNoIJ(ii, jj))) = basin potential evaporation
                        DRMET(NextNoIJ(ii, jj)) = DRMET(NextNoIJ(ii, jj)) + DRMIca(NextNoIJ(ii, jj)) - SumDRMIca! (DRMET(NextNoIJ(ii, jj)) + DRMIca(NextNoIJ(ii, jj))) = basin potential evaporation
                        SumDRMIca = 0.0
                    Else
                        SumDRMIca = SumDRMIca - (DRMET(NextNoIJ(ii, jj)) + DRMIca(NextNoIJ(ii, jj)))! (DRMET(NextNoIJ(ii, jj)) + DRMIca(NextNoIJ(ii, jj))) = basin potential evaporation
                        DRMET(NextNoIJ(ii, jj)) = 0.0
                    End If
                Else
                    DRMIca(NextNoIJ(ii, jj)) = DRMIca(NextNoIJ(ii, jj)) - DRMET(NextNoIJ(ii, jj))
                    SumDRMIca = SumDRMIca + DRMIca(NextNoIJ(ii, jj))
                    DRMET(NextNoIJ(ii, jj)) = 0.0
                Endif
                print *,"finished vegetation interception"
    
                
!================Effective precipitation
                If (RFC(ii, jj) .eq. 1 .Or. FlowDirection(ii, jj) .eq. 8) Then
                  Pnet(NextNoIJ(ii, jj)) = DRMPO(NextNoIJ(ii, jj)) - DRMET(NextNoIJ(ii, jj)) - DRMIca(NextNoIJ(ii, jj))
                Else
                    Pnet(NextNoIJ(ii, jj)) = DRMPO(NextNoIJ(ii, jj)) - DRMET(NextNoIJ(ii, jj)) - DRMIca(NextNoIJ(ii, jj))
                     If (Pnet(NextNoIJ(ii, jj)) .gt. 500) Then
                        Pnet(NextNoIJ(ii, jj)) = Pnet(NextNoIJ(ii, jj))
                    Endif
                Endif
    
!===================Prepare for evapotranspiration,starts the main part of XAJ model
                Pe = Pnet(NextNoIJ(ii, jj))+Ps      !precipitation from snow melting, directly into Pnet
                Ek = DRMET(NextNoIJ(ii, jj))
                GWM = DRMWM(ii, jj)
                GSM = DRMSM(ii, jj)
                GW = GridW(ii, jj)
                GS = GridS(ii, jj)
                GWU = GridWU(ii, jj)
                GWL = GridWL(ii, jj)
                GWD = GW - GWU - GWL
                Kg = DRMKg(ii, jj)
                Ki = DRMKi(ii, jj)
                                    
                KKi = (1. - ((1. - (Kg + Ki)) ** (DT / 24.))) / (1. + Kg / Ki)
                KKg = KKi * Kg / Ki
                Kg = KKg
                Ki = KKi 
                
                if (isnan(Kg)) then
                    Kg=0.2
                endif
                if (isnan(Ki)) then
                    Ki=0.5
                endif

                Grfc = RFC(ii, jj)! Judging whether pnet exsit in the grid
                Dp = DRMfc(ii, jj)
                ZUpper = AlUpper * ThickoVZ(ii, jj)
                ZLower = AlLower * ThickoVZ(ii, jj)
                ZDeeper = AlDeeper * ThickoVZ(ii, jj)
                
  !======================soil moisture and three-layer evapotranspiration              
                
               If (ZUpper .gt. 300) Then
                    GWUM = (STFC(SType030(ii, jj)) - STWP(SType030(ii, jj))) * 300. + (STFC(SType30100(ii, jj)) - STWP(SType30100(ii, jj))) * (ZUpper - 300.)
                    GWLM = (STFC(SType30100(ii, jj)) - STWP(SType30100(ii, jj))) * ZLower
                Else
                    GWUM = (STFC(SType030(ii, jj)) - STWP(SType030(ii, jj))) * ZUpper
                    If (ZUpper + ZLower .gt. 300.) Then
                        GWLM = (STFC(SType030(ii, jj)) - STWP(SType030(ii, jj))) * (300. - ZUpper) + (STFC(SType30100(ii, jj)) - STWP(SType30100(ii, jj))) * (ZLower - 300. + ZUpper)
                    Else
                        GWLM = (STFC(SType030(ii, jj)) - STWP(SType030(ii, jj))) * ZLower
                    Endif
                Endif
                GWDM = GWM - GWUM - GWLM
                
133           If (Pe .le. 0.0) Then
                    Grfc = 0.0
                    If (GWU + Pe .lt. 0.0) Then!Upper + lower + deeper
                        GEU = GWU + Ek + Pe
                        GWU = 0.0 
                        If (GWLM .lt. 0)   GWLM = 0.01
                        GEL = (Ek - GEU) * GWL / GWLM
                        If (GWL .lt. DeeperC * GWLM) Then
                            GEL = DeeperC * (Ek - GEU)
                        End If
                        If ((GWL - GEL)  .lt. 0.0) Then
                            ged = GEL - GWL
                            GEL = GWL
                            GWL = 0.
                            GWD = GWD - ged
                        Else
                            ged = 0.
                            GWL = GWL - GEL
                        endif
                    Else
                        GEU = Ek
                        GEL = 0.
                        ged = 0.
                        GWU = GWU + Pe
                   endif
                    GW = GWU + GWL + GWD
                    GE = GEU + GEL + ged
                    GRs = 0.
                    GRg = GS * Kg
                    GRi = GS * Ki
                    GS = GS * (1. - Kg - Ki)
134           Elseif  (Pe .gt. 0.0) then
                    GEU = Ek
                    GEL = 0.
                    ged = 0.
                    GE = Ek
                                     
!================Divide effecitve precipitation into several blocks(base on Dive=5mm)                    
                    nd = Int(Pe / Div) + 1.
                    If (Mod(Pe, Div) .eq. 0) Then
                        If (Pe - Int(Pe) .eq. 0) Then
                            nd = nd - 1
                        endif
                    endif     
                    
                    Allocate(PPe(nd))
                    do m = 1, nd - 1
                        PPe(m) = Div
                    enddo
                    
                    PPe(nd) = Pe - (nd - 1) * Div
                    GRs = 0.
                    GRg = 0.
                    GRi = 0.
                    KKi = (1.0 - (1.0 - (Kg + Ki))** (1.0 / nd)) / (Kg + Ki)
                    KKg = KKi * Kg
                    KKi = KKi * Ki    
                    
                    do 31 m = 1, nd
                        If (PPe(m) + GW  .lt. GWM) Then
                            Grfc = 0.
                            If (PPe(m) + GWU .lt. GWUM) Then
                                GWU = PPe(m) + GWU
                            ElseIf (GWL + GWUM - GWU + PPe(m)  .lt. GWLM) Then
                                GWL = GWL + GWUM - GWU + PPe(m)
                                GWU = GWUM
                            Else
                                GWD = GWD + PPe(m) - (GWUM - GWU) - (GWLM - GWL)
                                GWU = GWUM
                                GWL = GWLM
                            endif
                            GW = GWU + GWL + GWD
                            GRs = GRs
                            GRg = GRg + GS * KKg
                            GRi = GRi + GS * KKi
                            GS = GS * (1. - KKg - KKi)
                        Else
                            GR = PPe(m) + GW - GWM
                            GWU = GWUM
                            GWL = GWLM
                            GWD = GWDM
                            GW = GWM
                            Grfc = 1.! Saturated
                            If (GR + GS .le. GSM) Then
                                GRs = GRs
                                GS = GS + GR
                                GRg = GRg + GS * KKg
                                GRi = GRi + GS * KKi
                                GS = GS * (1. - KKg - KKi)
                            Else
                                GRs = GRs + GR + GS - GSM
                                GS = GSM
                                GRg = GRg + GSM * KKg
                                GRi = GRi + GSM * KKi
                                GS = GSM * (1. - KKg - KKi)
                            endif
                        endif
31                  continue
                     Deallocate(PPe)
135           endif
                           
                !===========汇流分配比例方案：1--单个网格分配；2--上游汇入叠加==== 
         if (Inrunmethod .eq. 2)  then
                GQi = GridQi(ii, jj)
                GQg = GridQg(ii, jj)
                GQs = GridQs(ii, jj)
                GQs = GQs * CCS1 + GRs * Ct * (1 - CCS1)
                Gqch = 0 
                GQi = GQi * Ci + GRi * Ct * (1. - Ci)
                GQg = GQg * Cg + GRg * Ct * (1. - Cg)
                
                GridQi(ii, jj) = GQi
                GridQg(ii, jj) = GQg
                GridQs(ii, jj) = GQs * (1 - Dp)
                GW = GWU + GWL + GWD
                GridW(ii, jj) = GW
                GridS(ii, jj) = GS
                If (GW .ge. GWM) Then
                    GridWM(ii, jj) = 1
                    NoWM(i) = NoWM(i) + 1
                Else
                    GridWM(ii, jj) = 0
                endif
                
                If (GS / (1. - KKg - KKi) .ge. GSM) Then
                    GridSM(ii, jj) = 1
                    NoSM(i) = NoSM(i) + 1
                Else
                    GridSM(ii, jj) = 0
                endif
                
                GridWU(ii, jj) = GWU
                GridWL(ii, jj) = GWL
                GridWD(ii, jj) = GWD
                RFC(ii, jj) = Grfc
                DRMET(NextNoIJ(ii, jj)) = GE
          endif
         
                
!=====================original code for flow direction=========================

                if ( FlowDirection(ii, jj) .eq. 0) then            
                    i1 = ii - 1
                    j1 = jj + 1
                 elseif ( FlowDirection(ii, jj) .eq. 1) then
                    i1 = ii
                    j1 = jj + 1
                  elseif ( FlowDirection(ii, jj) .eq. 2) then
                    i1 = ii + 1
                    j1 = jj + 1
                  elseif ( FlowDirection(ii, jj) .eq. 3) then
                    i1 = ii + 1
                    j1 = jj
                elseif ( FlowDirection(ii, jj) .eq. 4) then
                    i1 = ii + 1
                    j1 = jj - 1
                elseif ( FlowDirection(ii, jj) .eq. 5) then
                    i1 = ii
                    j1 = jj - 1
                elseif ( FlowDirection(ii, jj) .eq. 6) then
                    i1 = ii - 1
                    j1 = jj - 1
                elseif ( FlowDirection(ii, jj) .eq. 7) then
                    i1 = ii - 1
                    j1 = jj
               elseif ( FlowDirection(ii, jj) .eq. 8) then
                    i1 = ii
                    j1 = jj
               endif
                        
 !======================Muskingum routine for surface runoff               
         if (Inrunmethod  .eq. 2)  then
              
                InflowQs(2, NextNoIJ(ii, jj)) = GQs + SumQs(NextNoIJ(ii, jj))                   
                If (i .eq. 1) Then
                    OutflowQs(2, NextNoIJ(ii, jj)) = InflowQs(2, NextNoIJ(ii, jj))
                endif
                If (i  .gt. 1) Then
                    MDt = DT
                    MC1 = (MXs * MKs + 0.5 * MDt) / ((1 - MXs) * MKs + 0.5 * MDt)
                    MC2 = (0.5 * MDt - MXs * MKs) / ((1 - MXs) * MKs + 0.5 * MDt)
                    MC3 = ((1 - MXs) * MKs - 0.5 * MDt) / ((1 - MXs) * MKs + 0.5 * MDt) 
                    OutflowQs(2, NextNoIJ(ii, jj)) = MC1 * InflowQs(1, NextNoIJ(ii, jj)) + MC2 * InflowQs(2, NextNoIJ(ii, jj)) + MC3 * OutflowQs(1, NextNoIJ(ii, jj))
                    if (OutflowQs(2, NextNoIJ(ii, jj)) .gt. 0) then
                         OutflowQs(2, NextNoIJ(ii, jj)) =OutflowQs(2, NextNoIJ(ii, jj))
                    else
                        OutflowQs(2, NextNoIJ(ii, jj)) =0
                    endif
                    InflowQs(1, NextNoIJ(ii, jj)) = InflowQs(2, NextNoIJ(ii, jj))
                    OutflowQs(1, NextNoIJ(ii, jj)) = OutflowQs(2, NextNoIJ(ii, jj))
                endif
 
                If (FlowDirection(ii, jj) .eq. 8) Then
                    SumQs(NextNoIJ(i1, j1)) = OutflowQs(2, NextNoIJ(ii, jj))
                Else
                    SumQs(NextNoIJ(i1, j1)) = SumQs(NextNoIJ(i1, j1)) + OutflowQs(2, NextNoIJ(ii, jj)) !20231221
                endif            
         endif
               
                
!===============Inter flow accumulation(Muskingum routine)
                InflowQi(2, NextNoIJ(ii, jj)) = GQi + SumQi(NextNoIJ(ii, jj))
                
                If (i .eq. 1) Then
                    OutflowQi(2, NextNoIJ(ii, jj)) = InflowQi(2, NextNoIJ(ii, jj))
                endif
                
                If ( i  .gt. 1) Then
                    MDt = DT
                    MC1 = (MXi * MKi + 0.5 * MDt) / ((1 - MXi) * MKi + 0.5 * MDt)
                    MC2 = (0.5 * MDt - MXi * MKi) / ((1 - MXi) * MKi + 0.5 * MDt)
                    MC3 = ((1 - MXi) * MKi - 0.5 * MDt) / ((1 - MXi) * MKi + 0.5 * MDt)
                    OutflowQi(2, NextNoIJ(ii, jj)) = MC1 * InflowQi(1, NextNoIJ(ii, jj)) + MC2 * InflowQi(2, NextNoIJ(ii, jj)) + MC3 * OutflowQi(1, NextNoIJ(ii, jj))                  
                    if (OutflowQi(2, NextNoIJ(ii, jj)) .gt. 0.0) then
                        OutflowQi(2, NextNoIJ(ii, jj))=OutflowQi(2, NextNoIJ(ii, jj))
                    else
                        OutflowQi(2, NextNoIJ(ii, jj))=0.0
                    endif
                    InflowQi(1, NextNoIJ(ii, jj)) = InflowQi(2, NextNoIJ(ii, jj))
                    OutflowQi(1, NextNoIJ(ii, jj)) = OutflowQi(2, NextNoIJ(ii, jj))
                   endif
    
                If (FlowDirection(ii, jj) .eq. 8) Then
                    SumQi(NextNoIJ(i1, j1)) = OutflowQi(2, NextNoIJ(ii, jj))
                Else
                    SumQi(NextNoIJ(i1, j1)) = SumQi(NextNoIJ(i1, j1)) + OutflowQi(2, NextNoIJ(ii, jj))
                endif
                
!===========Ground water accumulation using Muskingum algorithm 
                InflowQg(2, NextNoIJ(ii, jj)) = GQg + SumQg(NextNoIJ(ii, jj))
                
                If (i .eq. 1) Then
                    OutflowQg(2, NextNoIJ(ii, jj)) = InflowQg(2, NextNoIJ(ii, jj))
                End If
                If (i  .gt. 1) Then
                    MDt = DT
                    MC1 = (MXg * MKg + 0.5 * MDt) / ((1 - MXg) * MKg + 0.5 * MDt)
                    MC2 = (0.5 * MDt - MXg * MKg) / ((1 - MXg) * MKg + 0.5 * MDt)
                    MC3 = ((1 - MXg) * MKg - 0.5 * MDt) / ((1 - MXg) * MKg + 0.5 * MDt)
                    OutflowQg(2, NextNoIJ(ii, jj)) = MC1 * InflowQg(1, NextNoIJ(ii, jj)) + MC2 * InflowQg(2, NextNoIJ(ii, jj)) + MC3 * OutflowQg(1, NextNoIJ(ii, jj))
                    if (OutflowQg(2, NextNoIJ(ii, jj)) .gt. 0.0) then   
                    OutflowQg(2, NextNoIJ(ii, jj)) = OutflowQg(2, NextNoIJ(ii, jj))
                    else
                      OutflowQg(2, NextNoIJ(ii, jj)) = 0.0
                    endif
                    InflowQg(1, NextNoIJ(ii, jj)) = InflowQg(2, NextNoIJ(ii, jj))
                    OutflowQg(1, NextNoIJ(ii, jj)) = OutflowQg(2, NextNoIJ(ii, jj))
                endif
                If (FlowDirection(ii, jj) .eq. 8) Then
                    SumQg(NextNoIJ(i1, j1)) = OutflowQg(2, NextNoIJ(ii, jj))
                Else
                    SumQg(NextNoIJ(i1, j1)) = SumQg(NextNoIJ(i1, j1)) + OutflowQg(2, NextNoIJ(ii, jj))
                !OutflowQch
                endif
               
!=================River channel routine 
                InflowQch(2, NextNoIJ(ii, jj)) = Gqch + SumQch(NextNoIJ(ii, jj))
                
                If (i  .eq. 1) Then
                    OutflowQch(2, NextNoIJ(ii, jj)) = InflowQch(2, NextNoIJ(ii, jj))
                endif
                If (i  .gt.  1) Then
                    MDt = DT
                    MC1 = (MXch * MKch + 0.5 * MDt) / ((1 - MXch) * MKch + 0.5 * MDt)
                    MC2 = (0.5 * MDt - MXch * MKch) / ((1 - MXch) * MKch + 0.5 * MDt)
                    MC3 = ((1 - MXch) * MKch - 0.5 * MDt) / ((1 - MXch) * MKch + 0.5 * MDt)
                    OutflowQch(2, NextNoIJ(ii, jj)) = MC1 * InflowQch(1, NextNoIJ(ii, jj)) + MC2 * InflowQch(2, NextNoIJ(ii, jj)) + MC3 * OutflowQch(1, NextNoIJ(ii, jj))
                    if (OutflowQch(2, NextNoIJ(ii, jj)) .gt. 0.0) then
                    OutflowQch(2, NextNoIJ(ii, jj)) =OutflowQch(2, NextNoIJ(ii, jj))
                    else
                   OutflowQch(2, NextNoIJ(ii, jj)) =0.0
                   endif
                    InflowQch(1, NextNoIJ(ii, jj)) = InflowQch(2, NextNoIJ(ii, jj))
                    OutflowQch(1, NextNoIJ(ii, jj)) = OutflowQch(2, NextNoIJ(ii, jj))
                endif  
                
                If (RiverPoint(i1, j1) .eq. 1) Then
                    If (FlowDirection(ii, jj) .eq. 8) Then
                        SumQch(NextNoIJ(i1, j1)) = OutflowQch(2, NextNoIJ(ii, jj)) !+ SumQi(NextNoIJ(i1, j1)) + SumQg(NextNoIJ(i1, j1))
                    Else
                        SumQch(NextNoIJ(i1, j1)) = SumQch(NextNoIJ(i1, j1)) + OutflowQch(2, NextNoIJ(ii, jj)) !+ SumQi(NextNoIJ(i1, j1)) + SumQg(NextNoIJ(i1, j1))
                    endif
                    endif                
                
                if (FlowDirection(ii, jj) .eq. 8) Then
                    Qoutch = SumQch(NextNoIJ(i1, j1))
                    Qouts = SumQs(NextNoIJ(i1, j1))
                    Qouti = SumQi(NextNoIJ(i1, j1))
                    Qoutg = SumQg(NextNoIJ(i1, j1))
                 
                endif
                QSimch(k, i + LagTime) =QSimch(k, i + LagTime - 1) * CCS + (Qoutch) * (1 - CCS)
                QSims(k, i + LagTime) = QSims(k, i + LagTime - 1) * CCS + (Qouts) * (1 - CCS)    
                QSimi(k, i + LagTime) = QSimi(k, i + LagTime - 1) * CCS + (Qouti) * (1 - CCS)
                QSimg(k, i + LagTime) = QSimg(k, i + LagTime - 1) * CCS + (Qoutg) * (1 - CCS)
                QSim(k, i + LagTime) = QSim(k, i + LagTime - 1) * CCS + (Qoutch + Qouts + Qouti + Qoutg) * (1 - CCS)              
83       enddo      !effective grid
           TotalPre(i,1)=TotalPre_rain/DRMGCNo
           TotalPre(i,2)=TotalPre_snow/DRMGCNo
           deallocate(GridPobs, GridTc, GridTf, GridDs)
                                
 !====================grid simulation flow for each time step=========================
                     do j=1,DRMGCNo
                         GridQSim(SortingRow(j),SortingCol(j),i)=QSim(j,i+LagTime)!+GridQSim(SortingRow(j),SortingCol(j),i)
                         GridQSimch(SortingRow(j),SortingCol(j),i)=QSimch(j,i+LagTime)
                         GridQSims(SortingRow(j),SortingCol(j),i)=QSims(j,i+LagTime)
                         GridQSimi(SortingRow(j),SortingCol(j),i)=QSimi(j,i+LagTime)!20231220
                         GridQSimg(SortingRow(j),SortingCol(j),i)=QSimg(j,i+LagTime)!20231220
                     enddo
                     
84  enddo    !each time step 

    
!=================save the state of soil moisture===============
    
2100 format(1x,4I10,3F15.2)
2101 format(1x,I10,3F12.3,3F10.2)
2102 format(1x,4I8, 3500F8.2)
2103 format(1x,4I10,20F15.2) 
2104 format(1x,3I10,8F15.4)
  
  
2048 enddo    !each sub basin
            
2200 format(1x,A20,I8)
2201 format(1x,A20,F12.5)
2202 format(1x,2000F10.2)
2203 format(1x,A280,I8) 


!=================output grid discharge for each time step========================
     allocate(GridQSimTemp(Nx,Ny), TotalQ_tmp(ntime1,NoStation*2+2),StaQSim(ntime1,NoStation))
     TotalQ_tmp=0.0
     StaQSim=0.0

     do kk=1,NoStation
     if (lonlat(kk,3) .ne. Nodata) then
        ii=Nx+1-ceiling((lonlat(kk,3)-yllcorner)/DDem)
        jj=ceiling((lonlat(kk,2)-xllcorner)/DDem)

        Do i=1+Nwarmday,TSteps
            TotalQ_tmp(i,1)=TotalPre(i,1)
            TotalQ_tmp(i,2)=TotalPre(i,2)
            TotalQ_tmp(i,2*kk-1+2)=TotalQobs(i,kk)
            TotalQ_tmp(i,2*kk+2)=GridQSim(ii,jj,i+LagTime1)
        enddo

     endif
     enddo

!====================daily QSim============================
 
     print *,'Saving results'
     open(55,file='.\output\StaQSim.txt')   
     do ii=1+Nwarmday,TSteps
         write(55,2104) IY2(ii),IM2(ii),ID2(ii),(TotalQ_tmp(ii,jj),jj=1,NoStation*2+2)
     enddo
     close(55)  
    end program iRainSnowHydro