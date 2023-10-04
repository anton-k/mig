# Anatomy of the response

## Http response

For short explanation sometimes things go bad and we would like to send errors.
For HTML we use the same type for errors and result values most of the time. It is an Html page.
But for JSON applications often errors would have different type than values.
And we have special type of response for that. That is why we would like to keep
the value type of the `Send` type general and not to restrict it as in HTML case.
