chan queue = [] of {}
int n;
byte taken[n];

proctype makeconfig(){
	int i,j;
	byte pos[n];
	byte cols_in_use[n+1];
	pos[0] = 0;
	i = 1;
	do
	:: (i<=n-1) ->  cols_in_use[i]=1;i++
	:: (i>n-1) -> break
	od;
	cols_in_use[n] = 0;
	i = 1;
	do 
	::(i<=n-1)-> j = i;
				 do
				 ::	(i>=0) -> if 
				 		  	  :: (pos[i] == pos[j]) -> pos[i]++;
													   if 
													   :: (pos[i]== n) -> pos[i]=0;
						 							   fi;
						 							   j=i;
						 							   i--
						   	  fi
				 :: (i<0) -> break 
				 od;
				 i++;
				 queue!(pos)
	:: (i>n-1)-> break
	od;
	i=n-1;
	do 
	::(i>=0)-> cols_in_use[i] = 0;
				 do
				 ::	(cols_in_use[pos[i]]==1) -> pos[i]++
				 :: (cols_in_use[pos[i]]==0 ) -> break 
				 od;
				 if 
				 :: (pos[i]<n) -> cols_in_use[pos[i]] = 1;goto exit_loop1;
				 fi;
				 i--		 					
	:: (i<0)-> break
	od;
exit_loop1:
	if
	:: (i<0) ->  queue!(pos); goto end_proc;
	fi;
	i++;
	j=0;
	do 
	::(j<=n-1)-> cols_in_use[i] = 0;
				 if 
				 :: (cols_in_use[j]==0) -> cols_in_use[j] = 1;pos[i] = j;i++
				 fi;
				 j++		 					
	:: (j>n-1)-> break
	od;

end_proc:
}



proctype chkconfig(){
	do
	::queue?data;process data here
	od
}

init {
	run makeconfig();run chkconfig()
}