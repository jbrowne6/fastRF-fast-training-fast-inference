runRF <- function(name, func, X=NULL,Y=NULL,FStrcut=NULL,numTrees=NULL,minParent=NULL,numCores=NULL){

	############### Training ###################################
	if(func == "train"){
		#### Check Params ####
		if(is.null(X)){
			stop("need training set (X)")
		}
		if(is.null(numTrees)){
			numTrees <- 100
		}
		if(is.null(minParent)){
			minParent <- 1
		}
		if(is.null(numCores)){
			numCores <- 1
		}


		if(name == "rerf"){
			if(is.null(Y)){
				stop("need observation labels (Y)")
			}
			if(!is.numeric(Y)){
				Y <- as.numeric(Y)-1
			}else	if(min(unique(Y)) == 1){
				Y <- Y-1
			}
			if(!is.matrix(X)){
				X<-as.matrix(X)
			}
			ptm<-proc.time()
			fpRerF(X=X,Y=Y,forestType="binnedBase",minParent=minParent,numTreesInForest=numTrees,numCores=numCores)
			ptm_hold <- (proc.time()-ptm)[3]
			return(list(forest=forest,trainingTime=ptm_hold))
		}else	if(name == "rborist"){
			if(!is.matrix(X)){
				X<-as.matrix(X)
			}
			ptm<-proc.time()
			forest <- Rborist(x=X,y=Y,minNode=2,minParent,nTree=numTrees,nThread=numCores,thinLeaves=TRUE)
			ptm_hold <- (proc.time()-ptm)[3]
			return(list(forest=forest,trainingTime=ptm_hold))
		}else{
			stop("unknown system name argument")
		}


	############### Predicting ###############################
	}else if(func == "predict"){
if(name == "rerf"){

		}else{
stop("unknown system name argument")
		}


	############### Error ###############################
	}else{
stop("unknown func argument")
	}
}
