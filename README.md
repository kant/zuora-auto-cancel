[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/02af78419f69421396f090fea607ac3c)](https://www.codacy.com/app/johnduffell/zuora-auto-cancel?utm_source=github.com&utm_medium=referral&utm_content=guardian/zuora-auto-cancel&utm_campaign=Badge_Coverage)
# zuora-auto-cancel

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/df83c14325bc4c29aeae7e529f49f8a9)](https://app.codacy.com/app/johnduffell/zuora-auto-cancel?utm_source=github.com&utm_medium=referral&utm_content=guardian/zuora-auto-cancel&utm_campaign=badger)

This is the reader revenue lambda API/orchestration layer.  But it has a bad name so far - TODO fix this. 

Please keep all the various README in this project up to date, and improve them!
There should be one in each project and anywhere else you think it's would help.

---

## philosophy
The general philosophy of this project is to keep things under review and don't be afraid to refactor things
especially the hard parts of the structure when you think they are wrong.
The PR review at the end will always let you know when people don't like the idea
but if you miss an opportunity to improve the structure, this hurts more in the long term than a good
change with poor naming.
Yes, small PRs are good, but that means small in terms of the meaning of the change, rather than small in terms
of lines touched.  If you split out some code into a separate subproject and rename an existing one,
that is a small change because I can explain it in one sentence.  Github's failings in terms
of displaying it concisely are not your failings in making it small!

Anything that isn't a line in the sand should be questioned and changed at will.
Anything that is should be questioned too.

## Guidelines in the sand (there should not be too many of these!)
- **good naming** - a good name promotes cohesion - it says more about what **shouldn't** be in the construct
than what should be.  If you have a catch all name like "common" or "helpers" in mind, think again.
- **effects separation** - to promote good reuse and testability, keep all side effects in one place, and only depend
on it from the top level handlers.  Effects should be minimal, and the top level handlers should mostly be wiring.
If there's any code in either that you feel could be unit tested, it should probably be in another project.
- **one jar per lambda** - minimise the size of the deployment artifact
- **minimise dependencies (aka liabilities)** on external libraries as we have to keep them up to date, also they increase the size of the artifact

---

## Quality matters
The testing story is not perfect at the moment!  You are responsible for getting your own changes into production safely.  However there are a few common situations you will encounter:

### code only changes
You can be confident when making code only changes.  Run `sbt test` to run code only tests (ignores other tests).
They will automatically run on your build of your branch and when you merge to master.

Add your new tests in the normal way, and don't forget to "Run with Coverage" in IntelliJ.

### system integration points including Config loading
These tests do not (YET!) run automatically.
If you deploy to PROD without checking this, the lambdas will deploy but not actually work.

You can run the tests that hit external services by running `sbt effectsTest:test`.
This would need suitable AWS credentials to get hold of the config as well as config file under .aws with the content:
```
[default]
region = eu-west-1
```
You can tag your integration new tests with `taggedAs EffectsTest`.  This ignores them from the standard `sbt test`.

### Testing post deployment to CODE/PROD
After deploy you can't relax until you know an HTTP request will hit your lambda, and that your lambda can access any external services.
Traiditonal health checking doesn't happen.  You can run it in CODE and PROD by running `sbt healthCheck:test` which should run the CODEPRODHealthCheck.
The PROD health checks are [called by RunScope](https://www.runscope.com/radar/wrb0ytfjy4a4) every 5 minutes.


### Testing things with side effects
If you are changing the ET emails, it is worth running EmailClientSystemTest with your own email address.

---

## Howto update config
At the moment we just use S3 for config.

If you're making an addition, you can just copy the file from S3 for PROD and CODE, then update and upload.
Check the version in the AwsS3.scala ConfigLoad object.
`aws s3 cp s3://gu-reader-revenue-private/membership/payment-failure-lambdas/CODE/payment-failure-lambdas.private.v<VERSION>.json /etc/gu/CODE/ --profile membership`
Then do the reverse to upload again.

If you're making a change that you only want to go live on deployment (and have the ability to roll back
with riffraff) then you can increment the version.  Make sure you upload the file with the new version,
otherwise it will break the existing lambda immediately.

Follow the same process to update the prod config, and for DEV there is no version number.

To check that you have done it correctly, run the ConfigLoaderSystemTest.
That will check the relevant version can be understood by the local version of the code.

Ideally this test should be automated and your PR shouldn't be mergable until the config is readable.

---

## structure
The main project aggregates all the sub projects from handlers and lib, so we can build and test them in one go.

**root** (auto cancel, payment failure and stripe hook)

[root handlers readme](handlers/root.md)

**Catalog Service**

[catalog-service](handlers/catalog-service)

**Digital Subscription Expiry**

[digital-subscription-expiry](handlers/digital-subscription-expiry)

**Identity Backfill**

[identity-backfill](handlers/identity-backfill)

**Identity Retention**

[identity-retention](handlers/identity-retention)

**Zuora Retention**

[zuora-retention](handlers/zuora-retention)

**Libraries**

[shared code](lib)
