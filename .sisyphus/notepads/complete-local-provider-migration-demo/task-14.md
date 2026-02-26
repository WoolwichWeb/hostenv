## Task 14 - README documentation update

- Updated `examples/local-provider-migration/README.md` to document the modern provider-service + comin flow.
- Added architecture overview with ASCII diagram showing provider-service, webhook, and comin interactions.
- Documented provider-service flow in 5 steps: plan generation, webhook trigger, deploy intent creation, comin activation, status reporting.
- Explained webhook mechanism including endpoint format, payload structure, and handler behavior.
- Documented comin integration for Git-based continuous deployment.
- Updated quickstart instructions to cover both wizard and automated modes with modern flow.
- Preserved prerequisites section with all required tools.
- Retained and expanded troubleshooting section with provider-service and webhook debugging.
- Preserved VM/network documentation.
- Added "How It Works" section explaining VM setup, provider-service startup, and deployment flow.
- Added "File Structure" section for clarity.
- Did NOT document legacy deploy approach (as required).

### Evidence
- `.sisyphus/evidence/task-14-readme-review.txt`

### QA Verification Results
- grep "provider-service": 14 matches ✅
- grep "webhook": 16 matches ✅
- grep "comin": 5 matches ✅
