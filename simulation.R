# Project of Multiagent Systems



# add current bid to order book
add2OrderBook <- function(id, price)
{
    # add price to order book
    orderBook <- rbind(orderBook, c(id, price))
    colnames(orderBook) <- c("id", "price")
    
    # sort order book
    orderBook <- orderBook[order(orderBook$price),]
    
    # cat("The current order book is:\n")
    # print(orderBook)
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
    # cat(sprintf("The current bidding price for agent %d is: %f\n", id, price))
    
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
    
    x = load("agent3.Rdata")
    agent3Acc <<- get(x[3])
    agent3Unacc <<- get(x[4])
    
    x = load("agent4.Rdata")
    agent4Acc <<- get(x[3])
    agent4Unacc <<- get(x[4])
    
    x = load("agent5.Rdata")
    agent5Acc <<- get(x[3])
    agent5Unacc <<- get(x[4])
    
    x = load("agent6.Rdata")
    agent6Acc <<- get(x[3])
    agent6Unacc <<- get(x[4])
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
        if (id == 4)
            p <- getP(price, agent4Acc, agent4Unacc)
        if (id == 5)
            p <- getP(price, agent5Acc, agent5Unacc)
        if (id == 6)
            p <- getP(price, agent6Acc, agent6Unacc)
        
        if (p > 0.6)
            p = p - 0.35
        
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
    if (p > 0.7)
        p = p - 0.3
    
    p <- p / (price + 45) * 500
    return(p)
}

# create statistics for GD method
accepted <<- c()
unaccepted <<- c()

# create agent history variable
agentHistory1 <<- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("iteration", "price", "max"))), stringsAsFactors=F,row.names = NULL)
agentHistory2 <<- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("iteration", "price", "max"))), stringsAsFactors=F,row.names = NULL)

# read in simulation data
readSimulationData()

agent1Win <<- c()
agent2Win <<- c()



# simulation function
simulate <- function()
{
    # create order book variable
    orderBook <<- data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("id", "price"))), stringsAsFactors=F,row.names = NULL)
    
    # simulation process
    for (i in 1:2)
    {
        cat(sprintf("%d-th round of bidding\n", i))
        
        #price1 <- readline(prompt = "Enter price for agent 1: ")
        #price1 <- as.numeric(price1)
        price1 <- zi(30, 50)
        #price1 <- agentGD(3)
        
        if(price1==-2)
            break;
        if(price1!=-1)
            bid(i, 1, price1)
        
        #price2 <- readline(prompt = "Enter price for agent 2: ")
        #price2 <- as.numeric(price2)
        #print(price2)
        price2 <- agentGD(1)
        if(price2==-2)
            break;
        if(price2 != -1)
            bid(i, 2, price2)
    }
    
    if(orderBook$id[length(orderBook$price)] == 1)
        agent1Win <<- c(agent1Win, orderBook$price[length(orderBook$price)])
    else
        agent2Win <<- c(agent2Win, orderBook$price[length(orderBook$price)])
    
}

for (i in c(1:3000))
    simulate()

mean(agent1Win)
mean(agent2Win)
length(agent1Win)
length(agent2Win)

recordGameHistory() # careful: can not run with readline() well
