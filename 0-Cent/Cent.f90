program Cent
	! Programma che implementa il metodo di Lax-Friedrichs
	! per un equazione di Burgers' ==> f(u) = u^2 / 2
	
	implicit none

	! Dichiarazione delle variabili
	real, allocatable   :: u(:), x(:), u_old(:), flux(:) ! vettore che conterrà i flussi numerici
	real				:: lambda, cfl
	integer				:: i, n, succ, j
	! Spazio
	real				:: x_left, x_right ! estremi intervallo
	real				:: dx, len_intervallo
	integer				:: n_celle
	! Tempo
	real				:: T, dt
	integer				:: n_timestep
	! Variabili necessarie per stampare su file i dati
	CHARACTER(LEN=40) 	:: nome_file        ! Name of the output files
	CHARACTER(LEN=4) 	:: cont_file , lettera

	! Inizializzazione degli estremi del dominio
	x_left  = -1.0
	x_right =  1.0
	len_intervallo = x_right - x_left
	n_celle = 1000
	dx = len_intervallo / n_celle
		
	! Inizializzazione delle variabili temporali 
	t  = 0.0  ! tempo iniziale
	dt = dx	/ 3						!0.05/4
	n_timestep = 1500
	
	! Allocazione delle variabili
	allocate (u(n_celle))
	allocate (u_old(n_celle))
	allocate (flux(n_celle+1))
	allocate (x(n_celle))
	
	! Discretizzazione dello spazio
	do i = 1,n_celle
		x(i) = x_left + dx * (i-1)
	end do
	
	CALL u_init (n_celle,u,x)
	
	! Implementazione del metodo di LxF
	! ( i: indice delle celle )
	! ( n: indice del passo temporale )
	
	lambda		= dt / dx
	write(*,*) 'dt e dx = ',dt,dx
	write(*,*) 'lambda = ', lambda
	
	do n=1,n_timestep
		t = t + dt ! incrementiamo il tempo
		u_old(:) = u(:) ! teniamo memorizzata la soluzione del passo temp. prec. n-1
		
		CALL N_FLUX_computation (n_celle, u_old, flux)
		
		do i=1,n_celle
			! Controllo sulla condizione CFL
			cfl = u(i) * lambda
			if ((cfl>1) .OR. (cfl<-1)) then
				write (*,*) 'La condizione CFL non è verificata: ', cfl
				write (*,*) 'Al passo ', n, 'la cella ', i, 'ha velocità ', u(i)
				STOP
			end if
			
			if (i .EQ. n_celle) then 
				succ = 1
			else	
				succ = i+1
			end if
								
			u(i)=  u_old(i) -  lambda * (flux(succ)-flux(i))
			
		end do
		
		! Stampa, su file di testo, dei risultati ottenuti
		if ((n-(n/150)*150  ) .EQ. 0) then ! stampa solo un passo ogni 20
			cont_file=lettera(n)
			nome_file='sol.'//cont_file
			OPEN(unit=2,FILE=nome_file,status='new',action='write')
			do j=1,n_celle
				WRITE(2,*) x(j), u(j)
			end do
			CLOSE(2)
		end if
			
		
	end do
	
	! Deallocazione delle variabili
	deallocate (u)
	deallocate (u_old)
	deallocate (flux)
	deallocate (x)
	

end program Cent



subroutine u_init (N,U,X)
	implicit none
	! Dichiarazione variabili 
	integer					:: i, N ! N: lunghezza array U e X
	real, dimension (N)		:: U, X ! X: dominio spaziale 
									! U: soluzione iniziale
	integer					:: iostatus
	integer, parameter		:: DatoIniziale = 1
	
	! CASO (1) : PERTURBAZIONE
!	do i=1,N
!		U(i) = 0.5 * exp ( -80 * X(i)**2 )
!		if ( (X(i)>-0.3).AND.(X(i)<-0.1) ) then
!			U(i) = U(i) + 0.5 
!		end if
!	end do
 	
    ! CASO (2) : PROBLEMA DI RIEMANN
!	 do i=1,N
!	 	if ( X(i)<0.0 ) then
!	 		U(i) = 0.5 
!	 		else
!	 			U(i) = 0.0
!	 	end if
!	 end do
	 
    ! CASO (3) : PROBLEMA DI RIEMANN - scalino centrato
!	 do i=1,N
!	 	if ( (X(i)<0.5) .AND. (X(i)>-0.5)) then
!	 		U(i) = 0.5 
!	 		else
!	 			U(i) = 0.0
!	 	end if
!	 end do
	 
	! CASO (4) : IMPULSO - funzione continua
	do i=1,N
		U(i) = 0.5 * exp ( -80 * X(i)**2 )
	end do
	
	! Plot della sluzione iniziale <----> Scrittura su file dei dati
	OPEN (unit=DatoIniziale, file='DatoIniziale', status='unknown', action='write')
		do i=1,N
			WRITE(DatoIniziale,*)  X(i), U(i)
		end do
	CLOSE (DatoIniziale)

end subroutine u_init


CHARACTER*4 FUNCTION lettera(k)
    IMPLICIT NONE
    
    CHARACTER ones,tens,hund,thou
    INTEGER :: k
    INTEGER :: iten, ione, ihund, ithou

    ithou=INT(k/1000)
    ihund=INT((k-(ithou*1000))/100)
    iten=INT((k-(ithou*1000)-(ihund*100))/10)
    ione=k-ithou*1000-ihund*100-iten*10
    ones=CHAR(ione+48)
    tens=CHAR(iten+48)
    hund=CHAR(ihunD+48)
    thou=CHAR(ithou+48)
    lettera=thou//hund//tens//ones

    RETURN
  END FUNCTION lettera


subroutine N_FLUX_computation (N,U,F)
	implicit none
	! Dichiarazione delle variabili locali
	integer				:: N, i, prec
	real, dimension(N)	:: U
	real, dimension(N+1):: F
	real				:: fL, fC

	do i=1,N
		if (i .EQ. 1) then 
			prec = N
		else	
			prec = i-1
		end if
		
		CALL FLUX (u(i),fC)
		CALL FLUX (u(prec),fL)
		! Espressione del flusso numerico per schema centrato
		F(i) = 0.5 * (fL + fC)
	end do
	
	! Supponendo un dominio periodico:
	F(N+1) = F(1) 

	
end subroutine N_FLUX_computation

subroutine FLUX (u,f)
	implicit none
	! Dichiarazione delle variabili locali
	real		:: u, f
	
	! Flusso per equazione di Burgers'
	f = 0.5 * u**2
	
end subroutine FLUX





