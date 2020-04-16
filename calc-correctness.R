#' Calculate correctness
#' 
#' Calculate correctness of the reconstructed
#' network.
NULL

#' Calculate correctness of Predicted Network Adjacency List
#' 
#' Calculate correctness of a predicted directed network 
#' against a given true directed network.
#' 
#' @param pred_net_adj_list Adjaceny list of the predicted
#' directed network.
#' 
#' @param true_net_adj_mx Adjacency matrix of the true
#' directed network.

# n = Number of nodes in each of the nets.
CalcCorrectnessNetPredDiListVsTrueDiMx <-function(
  pred_net_adj_list, 
  true_net_adj_mx, 
  Result, 
  n) {
  
  num_nodes <- nrow(true_net_adj_mx)
  
  num_pred_edges <- nrow(pred_net_adj_list)
  
  tr_pos <- 0
  tr_neg <- 0
  fl_pos <- 0
  fl_neg <- 0
  
  for (edge_idx in 1:num_pred_edges) {
    src_node <- 
      pred_net_adj_list[edge_idx, ]$regulatoryGene
    
    tgt_node <- 
      pred_net_adj_list[edge_idx, ]$targetGene
    
    if (true_net_adj_mx[src_node, tgt_node] == 1) {
      tr_pos <- (tr_pos + 1)
    } else {
      fl_pos <- (fl_pos + 1)
    }
    
    # > di_net_adj_list[1, ]
    # regulatoryGene targetGene   weight
    # 1             G3        G10 1.132096
  }
  base::rm(node_idx)
  
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      if(pred_net[i,j] == 1 )
      {
        if(pred_net[i,j] == true_net[i,j])
        {
          #True Positive
          TrPos = TrPos + 1	 	
        }
        else
        {
          FlPos = FlPos +1
        }
      }	   
      if(pred_net[i,j] == 0 )
      {
        if (pred_net[i,j] == true_net[i,j])
        {
          # True Negative
          TrNeg = TrNeg + 1
        }
        else
        {
          FlNeg = FlNeg +1
        }
        
      }
    }	
  }
  
  #------------------------------------------------------------
  # Begin: Calculate Performance Metrics
  #------------------------------------------------------------
  TPR <- TrPos/(TrPos + FlNeg)
  FPR <- FlPos/(FlPos + TrNeg)
  
  ## Calculate FDR
  FDR <- NULL
  if ((FlPos == 0) & (TrPos == 0)) {
    FDR <- 0
  } else {
    FDR <- FlPos/(FlPos + TrPos)
  }
  
  ## Calculate PPV
  PPV <- NULL
  if ((FlPos == 0) & (TrPos == 0)) {
    PPV <- 0
  } else {
    PPV <- TrPos/(TrPos + FlPos)
  }
  
  ACC <- (TrPos + TrNeg)/(TrPos + FlPos + TrNeg + FlNeg)
  
  ## Calculate F1-score
  F1 <- NULL
  if ((PPV == 0) & (TPR == 0)) {
    
    F1 <- 0
    ## Ref: https://github.com/dice-group/gerbil/wiki/Precision,-Recall-and-F1-measure
    
  } else {
    F1 <- 2 * PPV * TPR / (PPV + TPR)
  }
  
  ## Calculate MCC.
  ## '((TrPos == 0) & (FlPos == 0))' => Null graph.
  ## '((TrNeg == 0) & (FlNeg == 0))' => Complete graph.
  MCC <- NULL
  if (((TrPos == 0) & (FlPos == 0)) | ((TrNeg == 0) & (FlNeg == 0))) {
    
    MCC <- 0
    ## Ref: https://lettier.github.io/posts/2016-08-05-matthews-correlation-coefficient.html
    
  } else {
    MCC <- ((TrPos * TrNeg) - (FlNeg * FlPos)) / sqrt((TrPos + FlPos) * (TrPos + FlNeg) * (TrNeg + FlPos) * (TrNeg+FlNeg))
  }
  
  ## Calculate AUC under ROC
  # table <- minet::validate(pred_net, true_net)
  # AUC <- minet::auc.roc(table)
  #------------------------------------------------------------
  # End: Calculate Performance Metrics
  #------------------------------------------------------------
  
  Result[1, 1] <- TrPos
  Result[1, 2] <- TrNeg
  Result[1, 3] <- FlPos
  Result[1, 4] <- FlNeg
  Result[1, 5] <- TPR
  Result[1, 6] <- FPR
  Result[1, 7] <- FDR
  Result[1, 8] <- PPV
  Result[1, 9] <- ACC
  Result[1, 10] <- MCC
  Result[1, 11] <- F1
  # Result[1,8] <- AUC
  
  return (Result)
}