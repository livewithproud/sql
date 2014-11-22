function (formula, data, weights, subset, na.action = na.rpart, 
    method, model = FALSE, x = FALSE, y = TRUE, parms, control, 
    cost, ...) 
{
    Call <- match.call()
    if (is.data.frame(model)) {
        m <- model
        model <- FALSE
    }
    else {
        indx <- match(c("formula", "data", "weights", "subset"), 
            names(Call), nomatch = 0L)
        if (indx[1] == 0L) 
            stop("a 'formula' argument is required")
        temp <- Call[c(1L, indx)]
        temp$na.action <- na.action
        temp[[1L]] <- quote(stats::model.frame)
        m <- eval.parent(temp)
    }
    Terms <- attr(m, "terms")
    if (any(attr(Terms, "order") > 1L)) 
        stop("Trees cannot handle interaction terms")
    Y <- model.response(m)
    wt <- model.weights(m)
    if (any(wt < 0)) 
        stop("negative weights not allowed")
    if (!length(wt)) 
        wt <- rep(1, nrow(m))
    offset <- model.offset(m)
    X <- rpart.matrix(m)
    nobs <- nrow(X)
    nvar <- ncol(X)
    if (missing(method)) {
        method <- if (is.factor(Y) || is.character(Y)) 
            "class"
        else if (inherits(Y, "Surv")) 
            "exp"
        else if (is.matrix(Y)) 
            "poisson"
        else "anova"
    }
    if (is.list(method)) {
        mlist <- method
        method <- "user"
        init <- if (missing(parms)) 
            mlist$init(Y, offset, wt = wt)
        else mlist$init(Y, offset, parms, wt)
        keep <- rpartcallback(mlist, nobs, init)
        method.int <- 4L
        numresp <- init$numresp
        numy <- init$numy
        parms <- init$parms
    }
    else {
        method.int <- pmatch(method, c("anova", "poisson", "class", 
            "exp"))
        if (is.na(method.int)) 
            stop("Invalid method")
        method <- c("anova", "poisson", "class", "exp")[method.int]
        if (method.int == 4L) 
            method.int <- 2L
        init <- if (missing(parms)) 
            get(paste("rpart", method, sep = "."), envir = environment())(Y, 
                offset, , wt)
        else get(paste("rpart", method, sep = "."), envir = environment())(Y, 
            offset, parms, wt)
        ns <- asNamespace("rpart")
        if (!is.null(init$print)) 
            environment(init$print) <- ns
        if (!is.null(init$summary)) 
            environment(init$summary) <- ns
        if (!is.null(init$text)) 
            environment(init$text) <- ns
    }
    Y <- init$y
    xlevels <- .getXlevels(Terms, m)
    cats <- rep(0L, ncol(X))
    if (!is.null(xlevels)) 
        cats[match(names(xlevels), colnames(X))] <- unlist(lapply(xlevels, 
            length))
    extraArgs <- list(...)
    if (length(extraArgs)) {
        controlargs <- names(formals(rpart.control))
        indx <- match(names(extraArgs), controlargs, nomatch = 0L)
        if (any(indx == 0L)) 
            stop(gettextf("Argument %s not matched", names(extraArgs)[indx == 
                0L]), domain = NA)
    }
    controls <- rpart.control(...)
    if (!missing(control)) 
        controls[names(control)] <- control
    xval <- controls$xval
    if (is.null(xval) || (length(xval) == 1L && xval == 0L) || 
        method == "user") {
        xgroups <- 0L
        xval <- 0L
    }
    else if (length(xval) == 1L) {
        xgroups <- sample(rep(1L:xval, length = nobs), nobs, 
            replace = FALSE)
    }
    else if (length(xval) == nobs) {
        xgroups <- xval
        xval <- length(unique(xgroups))
    }
    else {
        if (!is.null(attr(m, "na.action"))) {
            temp <- as.integer(attr(m, "na.action"))
            xval <- xval[-temp]
            if (length(xval) == nobs) {
                xgroups <- xval
                xval <- length(unique(xgroups))
            }
            else stop("Wrong length for 'xval'")
        }
        else stop("Wrong length for 'xval'")
    }
    if (missing(cost)) 
        cost <- rep(1, nvar)
    else {
        if (length(cost) != nvar) 
            stop("Cost vector is the wrong length")
        if (any(cost <= 0)) 
            stop("Cost vector must be positive")
    }
    tfun <- function(x) if (is.matrix(x)) 
        rep(is.ordered(x), ncol(x))
    else is.ordered(x)
    labs <- sub("^`(.*)`$", "\\1", attr(Terms, "term.labels"))
    isord <- unlist(lapply(m[labs], tfun))
    storage.mode(X) <- "double"
    storage.mode(wt) <- "double"
    temp <- as.double(unlist(init$parms))
    if (!length(temp)) 
        temp <- 0
    rpfit <- .Call(C_rpart, ncat = as.integer(cats * (!isord)), 
        method = as.integer(method.int), as.double(unlist(controls)), 
        temp, as.integer(xval), as.integer(xgroups), as.double(t(init$y)), 
        X, wt, as.integer(init$numy), as.double(cost))
    nsplit <- nrow(rpfit$isplit)
    ncat <- if (!is.null(rpfit$csplit)) 
        nrow(rpfit$csplit)
    else 0L
    nodes <- nrow(rpfit$inode)
    if (nsplit == 0L) 
        xval <- 0L
    numcp <- ncol(rpfit$cptable)
    temp <- if (nrow(rpfit$cptable) == 3L) 
        c("CP", "nsplit", "rel error")
    else c("CP", "nsplit", "rel error", "xerror", "xstd")
    dimnames(rpfit$cptable) <- list(temp, 1L:numcp)
    tname <- c("<leaf>", colnames(X))
    splits <- matrix(c(rpfit$isplit[, 2:3], rpfit$dsplit), ncol = 5L, 
        dimnames = list(tname[rpfit$isplit[, 1L] + 1L], c("count", 
            "ncat", "improve", "index", "adj")))
    index <- rpfit$inode[, 2L]
    nadd <- sum(isord[rpfit$isplit[, 1L]])
    if (nadd > 0L) {
        newc <- matrix(0L, nadd, max(cats))
        cvar <- rpfit$isplit[, 1L]
        indx <- isord[cvar]
        cdir <- splits[indx, 2L]
        ccut <- floor(splits[indx, 4L])
        splits[indx, 2L] <- cats[cvar[indx]]
        splits[indx, 4L] <- ncat + 1L:nadd
        for (i in 1L:nadd) {
            newc[i, 1L:(cats[(cvar[indx])[i]])] <- -as.integer(cdir[i])
            newc[i, 1L:ccut[i]] <- as.integer(cdir[i])
        }
        catmat <- if (ncat == 0L) 
            newc
        else {
            cs <- rpfit$csplit
            ncs <- ncol(cs)
            ncc <- ncol(newc)
            if (ncs < ncc) 
                cs <- cbind(cs, matrix(0L, nrow(cs), ncc - ncs))
            rbind(cs, newc)
        }
        ncat <- ncat + nadd
    }
    else catmat <- rpfit$csplit
    if (nsplit == 0L) {
        frame <- data.frame(row.names = 1L, var = "<leaf>", n = rpfit$inode[, 
            5L], wt = rpfit$dnode[, 3L], dev = rpfit$dnode[, 
            1L], yval = rpfit$dnode[, 4L], complexity = rpfit$dnode[, 
            2L], ncompete = 0L, nsurrogate = 0L)
    }
    else {
        temp <- ifelse(index == 0L, 1L, index)
        svar <- ifelse(index == 0L, 0L, rpfit$isplit[temp, 1L])
        frame <- data.frame(row.names = rpfit$inode[, 1L], var = tname[svar + 
            1L], n = rpfit$inode[, 5L], wt = rpfit$dnode[, 3L], 
            dev = rpfit$dnode[, 1L], yval = rpfit$dnode[, 4L], 
            complexity = rpfit$dnode[, 2L], ncompete = pmax(0L, 
                rpfit$inode[, 3L] - 1L), nsurrogate = rpfit$inode[, 
                4L])
    }
    if (method.int == 3L) {
        numclass <- init$numresp - 2L
        nodeprob <- rpfit$dnode[, numclass + 5L]/sum(wt)
        temp <- pmax(1L, init$counts)
        temp <- rpfit$dnode[, 4L + (1L:numclass)] %*% diag(init$parms$prior/temp)
        yprob <- temp/rowSums(temp)
        yval2 <- matrix(rpfit$dnode[, 4L + (0L:numclass)], ncol = numclass + 
            1L)
        frame$yval2 <- cbind(yval2, yprob, nodeprob)
    }
    else if (init$numresp > 1L) 
        frame$yval2 <- rpfit$dnode[, -(1L:3L), drop = FALSE]
    if (is.null(init$summary)) 
        stop("Initialization routine is missing the 'summary' function")
    functions <- if (is.null(init$print)) 
        list(summary = init$summary)
    else list(summary = init$summary, print = init$print)
    if (!is.null(init$text)) 
        functions <- c(functions, list(text = init$text))
    if (method == "user") 
        functions <- c(functions, mlist)
    where <- rpfit$which
    names(where) <- row.names(m)
    ans <- list(frame = frame, where = where, call = Call, terms = Terms, 
        cptable = t(rpfit$cptable), method = method, parms = init$parms, 
        control = controls, functions = functions, numresp = init$numresp)
    if (nsplit) 
        ans$splits = splits
    if (ncat > 0L) 
        ans$csplit <- catmat + 2L
    if (nsplit) 
        ans$variable.importance <- importance(ans)
    if (model) {
        ans$model <- m
        if (missing(y)) 
            y <- FALSE
    }
    if (y) 
        ans$y <- Y
    if (x) {
        ans$x <- X
        ans$wt <- wt
    }
    ans$ordered <- isord
    if (!is.null(attr(m, "na.action"))) 
        ans$na.action <- attr(m, "na.action")
    if (!is.null(xlevels)) 
        attr(ans, "xlevels") <- xlevels
    if (method == "class") 
        attr(ans, "ylevels") <- init$ylevels
    class(ans) <- "rpart"
    ans
}