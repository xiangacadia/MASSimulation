# Project of Multiagent Systems



# add current bid to order book
add2OrderBook <- function(id, price)
{
    # add price to order book
    orderBook <- rbind(orderBook, c(id, price))
    colnames(orderBook) <- c("id", "price")
    
    # sort order book
    orderBook <- orderBook[order(orderBook$price),]
    
    cat("The current order book is:\n")
    print(orderBook)
    return(orderBook)
}

# get current max bidding price
getMaxPrice <- function()
{
    return(max(orderBook$price))
}


# bid action
bid <- function(iteration, id, price)
{
    cat(sprintf("The current bidding price for agent %d is: %f\n", id, price))
    
    # add current bid to order book
    orderBook <<- add2OrderBook(id, price)
    
    # add current bid to agent history
    max <- getMaxPrice()
    
    if(id == 1)
    {
        agentHistory1 <<- rbind(agentHistory1, c(iteration, price, max))
        colnames(agentHistory1) <<- c("iteration", "price", "max")
    } else if(id == 2)
    {
        agentHistory2 <<- rbind(agentHistory2, c(iteration, price, max))
        colnames(agentHistory2) <<- c("iteration", "price", "max")
    }
    
}


# Zero-Intelligence strategy
zi <- function(min, max)
{
    return(sample(c(min:max), 1))
}

# record game history for GD
recordGameHistory <- function()
{
    accepted <<- c(accepted, orderBook$price[length(orderBook$price)])
    
    for (i in c(1:length(orderBook$price))-1)
    {
        unaccepted <<- c(unaccepted, orderBook$price[i])
    }
}

# save simulation data
saveSimulation <- function(fileName)
{
    save(agentHistory1, agentHistory2, accepted, unaccepted, file=fileName)
}

# read simulation data
readSimulationData <- function()
{
    x = load("agent1.Rdata")
    agent1Acc <<- get(x[3])
    agent1Unacc <<- get(x[4])
    
    x = load("agent2.Rdata")
    agent2Acc <<- get(x[3])
    agent2Unacc <<- get(x[4])
}

# bidding strategy for agent 1
agentGD <- function(id)
{
    prices <- c(30:50)
    pricePool <- c()
    
    for(price in prices)
    {
        if (id == 1)
            p <- getP(price, agent1Acc, agent1Unacc)
        if (id == 2)
            p <- getP(price, agent2Acc, agent2Unacc)
        if (id == 3)
            p <- getP(price, agent3Acc, agent3Unacc)
        
        if (p > 0.7)
            p = p - 0.3
        
        temp <- rep(price, as.integer(p * 100))
        pricePool <- c(pricePool, temp)
    }

    return (sample(pricePool, 1))
}


# compute the probability for a given price
getP <- function(price, faccepted, funaccepted)
{
    abl <- length(faccepted[faccepted < price])
    ubg <- length(funaccepted[funaccepted > price])
    p <- abl / (abl + ubg)
    return(p)
}

# create statistics for GD method
accepted <<- c()
unaccepted <<- c()

# create agent history variable
agentHistory1 <<- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("iteration", "price", "max"))), stringsAsFactors=F,row.names = NULL)
agentHistory2 <<- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("iteration", "price", "max"))), stringsAsFactors=F,row.names = NULL)

# create order book variable
orderBook <<- data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("id", "price"))), stringsAsFactors=F,row.names = NULL)

for (i in 1:5)
{
    cat(sprintf("%d-th round of bidding\n", i))
    
    price1 <- readline(prompt = "Enter price for agent 1: ")
    price1 <- as.numeric(price1)
    #price1 <- zi(30, 50)
    if(price1==-2)
        break;
    if(price1!=-1)
        bid(i, 1, price1)
    
    price2 <- readline(prompt = "Enter price for agent 2: ")
    price2 <- as.numeric(price2)
    print(price2)
    if(price2==-2)
        break;
    if(price2 != -1)
        bid(i, 2, price2)
}

recordGameHistory()
