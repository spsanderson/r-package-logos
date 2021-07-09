# hypercube.R
# code to rotate arbitrary d-dimensional hypercubes

# function definitions
makevertices=function(d)
{
    numv=2^d
    vertices=matrix(0,nrow=d,ncol=numv)
    for (j in 1:numv) {
        count=j-1
        for (i in 1:d) {
            vertices[i,j]=count%%2
            count=count%/%2
        }	
    }
    vertices
}

makeedges=function(vertices)
{
    d=dim(vertices)[1]
    numv=dim(vertices)[2]
    edges=NULL
    for (j in 1:numv) {
        for (i in j:numv) {
            if (sum(abs((vertices[,i]-vertices[,j])))==1) {
                edges=cbind(edges,c(i,j))
            }
        }
    }
    edges
}

rotate=function(vertices,angle=0.2)
{
    d=dim(vertices)[1]
    axes=sort(sample(1:d,2))
    rotmat=diag(rep(1,d))
    rotmat[axes[1],axes[1]]=cos(angle)
    rotmat[axes[2],axes[1]]=sin(angle)
    rotmat[axes[1],axes[2]]=-sin(angle)
    rotmat[axes[2],axes[2]]=cos(angle)
    vertices=rotmat %*% vertices
    vertices
}

plotcube=function(vertices,edges,col=2,lwd=2)
{
    d=dim(vertices)[1]
    plot(
        NULL
        ,xlim=c(-2,2)
        ,ylim=c(-2,2)
        # ,main=paste(d,"-d hypercube",sep="")
        ,xlab=""
        ,ylab=""
        , axes = FALSE
    )
    for (i in 1:dim(edges)[2]) {
        lines(c(vertices[1,edges[1,i]],vertices[1,edges[2,i]]),c(vertices[2,edges[1,i]],vertices[2,edges[2,i]]),col=col,lwd=lwd)
    }
}

rotationplot=function(vertices,edges,rotations=20,...)
{
    for (count in 1:rotations) {
        vertices=rotate(vertices,...)
        plotcube(vertices,edges)
    }
}

hypercube=function(d=4,...)
{
    vertices=makevertices(d)
    edges=makeedges(vertices)
    vertices=2*vertices
    vertices=vertices-1
    rotationplot(vertices,edges,...)
}

# examples

 hypercube()
 hypercube(3)
 hypercube(4,angle=0.1)
 hypercube(5,rotations=1)

# eof