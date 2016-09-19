void expMAc (double *ema, double *x, double *order, int *n)
{
	const double m = 2 / ( (*order) + 1 );

	for ( int i = 1; i < (*n); i++ ) {
		ema[i] = (x[i] - ema[i - 1]) * m + ema[i - 1];
	}
}