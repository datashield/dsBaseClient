// Shared by check.yaml / lint.yaml / dsBaseClient_test_suite.yaml's "Post PR
// comment" steps. Each workflow owns a subset of the markers below and only
// ever replaces its own, so the three runs (independent workflows, no
// ordering guarantee) converge on one combined PR comment regardless of
// which finishes first. The headline is recomputed from the row markers'
// current state on every update.

const ROW_KEYS = ['row:check', 'row:lint', 'row:tests-armadillo', 'row:tests-opal', 'row:coverage'];
const TOP_MARKER = '<!-- dsbaseclient-ci:summary -->';

const SKELETON = [
  TOP_MARKER,
  '<!-- headline -->⏳ Running checks...<!-- /headline -->',
  '',
  'Tested against dsBase versions:',
  'Armadillo: <!-- ver:armadillo -->_pending_<!-- /ver:armadillo -->',
  'Opal: <!-- ver:opal -->_pending_<!-- /ver:opal -->',
  '',
  '<table>',
  '<thead><tr><th>Check</th><th>Result</th></tr></thead>',
  '<tbody>',
  '<!-- row:check --><tr><td>Devtools checks</td><td>⏳ pending</td></tr><!-- /row:check -->',
  '<!-- row:lint --><tr><td>Code quality</td><td>⏳ pending</td></tr><!-- /row:lint -->',
  '<!-- row:tests-armadillo --><tr><td>Armadillo unit tests</td><td>⏳ pending</td></tr><!-- /row:tests-armadillo -->',
  '<!-- row:tests-opal --><tr><td>Opal unit tests</td><td>⏳ pending</td></tr><!-- /row:tests-opal -->',
  '<!-- row:coverage --><tr><td>Test coverage</td><td>⏳ pending</td></tr><!-- /row:coverage -->',
  '</tbody>',
  '</table>',
  '',
  'Logs: <!-- log:check -->_pending_<!-- /log:check --> &middot; <!-- log:lint -->_pending_<!-- /log:lint --> &middot; <!-- log:tests-armadillo -->_pending_<!-- /log:tests-armadillo --> &middot; <!-- log:tests-opal -->_pending_<!-- /log:tests-opal --> &middot; <!-- log:coverage -->_pending_<!-- /log:coverage -->'
].join('\n');

function replaceMarker(body, key, content) {
  const re = new RegExp(`<!-- ${key} -->[\\s\\S]*?<!-- /${key} -->`);
  const replacement = `<!-- ${key} -->${content}<!-- /${key} -->`;
  return re.test(body) ? body.replace(re, replacement) : body;
}

function computeHeadline(body) {
  let pass = 0, fail = 0, pending = 0;
  for (const key of ROW_KEYS) {
    const re = new RegExp(`<!-- ${key} -->([\\s\\S]*?)<!-- /${key} -->`);
    const m = body.match(re);
    const text = m ? m[1] : '';
    if (text.includes('❌')) fail++;
    else if (text.includes('✅')) pass++;
    else pending++;
  }
  if (fail > 0) return `❌ ${fail} of ${ROW_KEYS.length} checks failed`;
  if (pending > 0) return `⏳ ${pass} of ${ROW_KEYS.length} checks reported so far`;
  return `✅ All ${ROW_KEYS.length} checks passed`;
}

module.exports = async function postCiComment({ github, context, updates }) {
  let prNumber = context.payload.pull_request?.number;
  if (!prNumber) {
    const branch = context.ref.replace('refs/heads/', '');
    const prs = await github.rest.pulls.list({
      owner: context.repo.owner, repo: context.repo.repo,
      head: `${context.repo.owner}:${branch}`, state: 'open'
    });
    prNumber = prs.data[0]?.number;
  }
  if (!prNumber) return;

  const comments = await github.rest.issues.listComments({
    owner: context.repo.owner, repo: context.repo.repo, issue_number: prNumber
  });
  const existing = comments.data.find(c => c.body.includes(TOP_MARKER));

  // A comment from before this skeleton's format changed won't contain our
  // current markers - patching it would silently no-op every replace below.
  // Reset it to a fresh skeleton (still updating the same comment in place,
  // not creating a new one) rather than leaving stale content untouched.
  const isCompatible = existing && ROW_KEYS.some(key => existing.body.includes(`<!-- ${key} -->`));
  let body = isCompatible ? existing.body : SKELETON;
  for (const [key, content] of Object.entries(updates)) {
    body = replaceMarker(body, key, content);
  }
  body = replaceMarker(body, 'headline', computeHeadline(body));

  if (existing) {
    await github.rest.issues.updateComment({
      owner: context.repo.owner, repo: context.repo.repo, comment_id: existing.id, body
    });
  } else {
    await github.rest.issues.createComment({
      owner: context.repo.owner, repo: context.repo.repo, issue_number: prNumber, body
    });
  }
};
