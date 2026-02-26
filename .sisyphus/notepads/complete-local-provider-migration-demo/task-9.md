# Task 9: Webhook Trigger Mechanism

## Summary
Created a standalone webhook trigger script that simulates GitLab webhook calls to the hostenv provider service.

## Files Created
- `examples/local-provider-migration/trigger-webhook.sh` - Script to trigger webhook deployments

## What the Script Does
1. Extracts project hash from `plan.json` (looks for `projectNameHash` field)
2. POSTs to `/webhook/<hash>` endpoint using Unix socket
3. Uses `Content-Type: application/octet-stream` (required by webhook handler)
4. Sends JSON payload with commit SHA information
5. Verifies response contains `accepted: true` and `jobId`
6. Optionally verifies deploy intent creation via `/api/deploy-intents/by-sha`

## Usage
```bash
./trigger-webhook.sh --workdir /tmp/hostenv-demo --commit-sha abc123
```

## Key Implementation Details

### Webhook Handler Requirements
- Endpoint: `POST /webhook/<projectNameHash>`
- Content-Type: `application/octet-stream` (webhook handler expects raw bytes for signature verification)
- Body: JSON payload with commit information
- Response: `202 Accepted` with `{"accepted": true, "jobId": "..."}`

### Project Hash Resolution
The script reads `plan.json` and extracts the first `projectNameHash` found:
```bash
jq -r '.environments[]?.hostenv?.projectNameHash // empty' "$PLAN_PATH" | head -1
```

### Plan.json Format
The plan.json must include:
```json
{
  "environments": {
    "main": {
      "type": "production",
      "node": "node-a",
      "hostenv": {
        "organisation": "acme",
        "project": "demo",
        "projectNameHash": "demohash"
      }
    }
  }
}
```

## Testing Evidence

### Successful Webhook Trigger
```
[21:21:55] extracted project hash from plan.json: demohash
[21:21:55] triggering webhook deployment
[21:21:55]   project hash: demohash
[21:21:55]   commit SHA: testcommit999
[21:21:55]   socket: /tmp/hostenv-provider-service-test-asOyK8/runtime/hostenv-provider.sock
[21:21:55] webhook accepted, jobId: 5207a6f7-0052-b3e2-5453-19dfd5d82959
```

### Response Format
```json
{
  "accepted": true,
  "jobId": "5207a6f7-0052-b3e2-5453-19dfd5d82959"
}
```

### Evidence Files
- `/home/liam/code/hostenv/.sisyphus/evidence/task-9-webhook-trigger.log`
- `/home/liam/code/hostenv/.sisyphus/evidence/task-9-deploy-intent-verify.log`

## Notes
- Does NOT implement signature verification (as per requirements)
- Does NOT require external GitLab access
- Deploy intent verification may return 404 initially (async processing)
- Webhook handler creates deploy intents in database via background job

## Constraints Followed
- ✅ Standalone script created
- ✅ Project hash calculated from plan.json
- ✅ POST to webhook endpoint with curl
- ✅ Response verified for jobId
- ✅ Evidence written to `.sisyphus/evidence/task-9-*.log`
- ✅ No signature verification implemented
- ✅ No external GitLab access required
- ✅ No provider-service code modified
