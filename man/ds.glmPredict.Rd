% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.glmPredict.R
\name{ds.glmPredict}
\alias{ds.glmPredict}
\title{Applies predict.glm() to a serverside glm object}
\usage{
ds.glmPredict(
  glmname = NULL,
  newdataname = NULL,
  output.type = "response",
  se.fit = FALSE,
  dispersion = NULL,
  terms = NULL,
  na.action = "na.pass",
  newobj = NULL,
  datasources = NULL
)
}
\arguments{
\item{glmname}{is a character string identifying the glm object on serverside to
which predict.glm is to be applied. Equivalent to <object> argument in native R's
predict.glm which is described as: a fitted object of class inheriting from 'glm'.}

\item{newdataname}{is a character string identifying an (optional) dataframe on
the serverside in which to look for new covariate values with which to predict.
If omitted,
the original fitted linear predictors from the original glm fit are used as the
basis of prediction. Precisely equivalent to the <newdata> argument in the
predict.glm function in native R.}

\item{output.type}{a character string taking the values 'response', 'link'
or 'terms'. The value 'response' generates predictions on the scale of the
original outcome, e.g. as proportions in a logistic regression. These
are often called 'fitted values'. The value
'link' generates predictions on the scale of the linear predictor, e.g.
log-odds in logistic regression, log-rate or log-count in Poisson regression.
The predictions using 'response' and 'link' are identical for a standard
Gaussian model with an identity link. The value 'terms' returns either
fitted values or predicted values on the link scale based not on
the whole linear predictor but on separate 'terms'. So, if age
is modelled as a five level factor, one of the output components
will relate to predictions (fitted values or link scale predictions) based on
all five levels of age simultaneously. Any simple covariate (e.g. not a composite
factor) will be treated as a term in its own right. ds.glmPredict's <output.type>
argument is precisely equivalent to the <type> argument in
native R's predict.glm function.}

\item{se.fit}{logical if standard errors for the fitted predictions are required.
Defaults to FALSE when the output contains only a vector (or vectors) of predicted
values. If TRUE, the output also contains corresponding vectors for the standard
errors of the predicted values, and a single value reporting the scale parameter
of the model. ds.glmPredict's <se.fit> argument is precisely equivalent to
the corresponding argument in predict.glm in native R.
argument is equivalent to the <type> argument in native R's predict.glm function.}

\item{dispersion}{numeric value specifying the dispersion of the GLM fit to be assumed
in computing the standard errors. If omitted, that returned by
summary applied to the glm object is used. e.g. if <dispersion> is unspecified
the dispersion assumed for a logistic regression or Poisson model is 1. But if
dispersion is set to 4, the standard errors of the predictions will all be
multiplied by 2 (i.e. sqrt(4)). This is useful in making predictions from
models subject to overdispersion. ds.glmPredict's <dispersion> argument
is precisely equivalent to the corresponding argument in predict.glm in native R.}

\item{terms}{a character vector specifying a subset of terms to return in the
prediction. Only applies if output.type='terms'. ds.glmPredict's <terms>
argument is precisely equivalent to the corresponding argument in predict.glm
in native R.}

\item{na.action}{character string determining what should be done with missing
values in the data.frame identified by <newdataname>. Default is na.pass which
predicts from the specified new data.frame with all NAs left in place. na.omit
removes all rows containing NAs. na.fail stops the function if there are any
NAs anywhere in the data.frame. For further details see help in native R.}

\item{newobj}{a character string specifying the name of the serverside object
to which the output object from the call to ds.glmPredict is to be written
in each study. If no <newobj> argument is specified, the output
object on the serverside defaults to the name "predict_glm".}

\item{datasources}{specifies the particular 'connection object(s)' to use.
e.g. if you have several data sets in the sources you are working with
called opals.a, opals.w2, and connection.xyz, you can choose which of
these to work with. The call 'datashield.connections_find()' lists all of
the different datasets available and if one of these is called 'default.connections'
that will be the dataset used by default if no other dataset is specified. If you
wish to change the connections you wish to use by default the call
datashield.connections_default('opals.a') will set 'default.connections'
to be 'opals.a' and so in the absence of specific instructions to the contrary
(e.g. by specifiying a particular dataset to be used via the <datasources>
argument) all subsequent function calls will be to the datasets held in opals.a.
If the <datasources> argument is specified, it should be set without
inverted commas: e.g. datasources=opals.a or datasources=default.connections.
The <datasources> argument also allows you to apply a function solely to a subset
of the studies/sources you are working with. For example, the second source
in a set of three, can be specified using a call such as datasources=connection.xyz[2].
On the other hand, if you wish to specify solely the first and third sources, the
appropriate call will be datasources=connections.xyz[c(1,3)]}
}
\value{
ds.glmPredict calls the serverside assign function glmPredictDS.as
which writes a new object to the serverside containing output precisely equivalent to
predict.glm in native R. The name for this serverside object is given by
the newobj argument or if that argument is missing or null it is called "predict_glm".
In addition, ds.glmPredict calls the serverside aggregate function glmPredictDS.ag
which returns an object containing non-disclosive summary statistics relating
either to a single prediction vector called fit or, if se.fit=TRUE, of two vectors
'fit' and 'se.fit' - the latter containing the standard errors of the predictions
in 'fit'. The non-disclosive summary statistics for the vector(s) include:
length, the total number of valid (non-missing) values,
the number of missing values, the mean and standard deviation of the valid
values and the 5%, 10%, 25%, 50%, 75%, 90%, and 95% quantiles of these values. In addition,
the output always includes: the name of the serverside glm object being predicted from,
the name - if one was specified - of the dataframe being used as the basis for predictions,
the output.type specified ('link', 'response' or 'terms'), the value of the
dispersion parameter if one had been specified and the residual scale parameter (which is
multipled by sqrt(dispersion parameter) if one has been set). If output.type = 'terms',
the summary statistics for the fit and se.fit vectors are replaced by equivalent
summary statistics for each column in fit and se.fit matrices which each have k columns
if k terms are being summarised.
}
\description{
Applies native R's predict.glm() function to a serverside
glm object previously created using ds.glmSLMA.
}
\details{
Clientside function calling a single assign function (glmPredictDS.as)
and a single aggregate function (glmPredictDS.ag). ds.glmPredict applies
the native R predict.glm function to a
glm object that has already been created on the serverside by fitting ds.glmSLMA.
This is precisely the same as the glm object created in native R by fitting a glm
using the glm function. Crucially, if ds.glmSLMA was originally applied to
multiple studies the glm object created on each study is based solely
on data from that study. ds.glmPredict has two distinct actions. First, the
call to the assign function applies the standard predict.glm function of
native R to the glm object on the serverside and writes all the output
that would normally be generated by predict.glm to a newobj on the serverside.
Because no critical information is passed to the clientside, there are no
disclosure issues associated with this action. Any standard DataSHIELD functions
can then be applied to the newobj to interpret the output. For example, it could
be used as the basis for regression diagnostic plots. Second, the call
to the aggregate function creates a non-disclosive summary of all the
information held in the newobj created by the assign function 
and returns this summary to the clientside.  For example, the full list of
predicted/fitted values generated by the model could be disclosive.
So although the newobj holds the full vector of fitted values, only the
total number of values, the total number of valid (non-missing) values,
the number of missing values, the mean and standard deviation of all valid
values and the 5%, 10%, 25%, 50%, 75%, 90%, and 95% quantiles of these values
are returned to the clientside by the aggregate function. The non-DataSHIELD
arguments of ds.glmPredict are precisely the equivalent to those of predict.glm
in native R and so all detailed information can be found using help(predict.glm)
in native R.
}
\author{
Paul Burton, for DataSHIELD Development Team 13/08/20
}
