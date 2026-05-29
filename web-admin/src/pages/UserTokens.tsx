import { useEffect, useState } from "react";
import {
  Card,
  Table,
  Button,
  Space,
  Tag,
  Modal,
  Form,
  Input,
  InputNumber,
  Select,
  Typography,
  App,
  Alert,
  Switch,
} from "antd";
import { useNavigate, useParams } from "react-router-dom";
import { adminApi, Token, User } from "../api/client";

export default function UserTokens() {
  const { id = "" } = useParams();
  const nav = useNavigate();
  const { message, modal } = App.useApp();
  const [user, setUser] = useState<User | null>(null);
  const [tokens, setTokens] = useState<Token[]>([]);
  const [includeRevoked, setIncludeRevoked] = useState(false);
  const [open, setOpen] = useState(false);
  const [mintedToken, setMintedToken] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [form] = Form.useForm();

  const refresh = async () => {
    setLoading(true);
    try {
      const [u, ts] = await Promise.all([
        adminApi.getUser(id),
        adminApi.listTokens(id, includeRevoked),
      ]);
      setUser(u);
      setTokens(ts);
    } catch (e: any) {
      message.error(`Load failed: ${e?.message}`);
    } finally {
      setLoading(false);
    }
  };
  useEffect(() => {
    refresh();
  }, [id, includeRevoked]);

  const onMint = async (vals: any) => {
    try {
      const out = await adminApi.mintToken({
        user_id: id,
        name: vals.name,
        scopes: vals.scopes ?? [],
        ttl: vals.ttl,
      });
      setMintedToken(out.token);
      setOpen(false);
      form.resetFields();
      refresh();
    } catch (e: any) {
      message.error(`Mint failed: ${e?.response?.data?.message ?? e?.message}`);
    }
  };

  const onRevoke = async (t: Token) => {
    modal.confirm({
      title: `Revoke token "${t.name}"?`,
      content: "This token will be rejected on all future requests.",
      okType: "danger",
      onOk: async () => {
        try {
          await adminApi.revokeToken(t.jti);
          message.success("Revoked");
          refresh();
        } catch (e: any) {
          message.error(`Failed: ${e?.message}`);
        }
      },
    });
  };

  const onReactivate = async (t: Token) => {
    modal.confirm({
      title: `Reactivate token "${t.name}"?`,
      content:
        "The token's original value becomes valid again on all future " +
        "requests (until it expires). No new token is shown — the " +
        "original string was only displayed once at creation.",
      onOk: async () => {
        try {
          await adminApi.reactivateToken(t.jti);
          message.success("Reactivated");
          refresh();
        } catch (e: any) {
          message.error(
            `Failed: ${e?.response?.data?.message ?? e?.message}`,
          );
        }
      },
    });
  };

  const onDelete = async (t: Token) => {
    modal.confirm({
      title: `Delete token "${t.name}"?`,
      content:
        "This permanently removes the token record. This cannot be undone, " +
        "and frees the name to be used again.",
      okType: "danger",
      okText: "Delete",
      onOk: async () => {
        try {
          await adminApi.deleteToken(t.jti);
          message.success("Deleted");
          refresh();
        } catch (e: any) {
          message.error(`Failed: ${e?.message}`);
        }
      },
    });
  };

  return (
    <>
      <Card
        title={user ? `Tokens for ${user.username}` : "Tokens"}
        extra={
          <Space>
            <span>
              <Switch
                size="small"
                checked={includeRevoked}
                onChange={setIncludeRevoked}
                data-testid="tokens-include-revoked"
              />{" "}
              show revoked
            </span>
            <Button onClick={() => nav("/users")}>Back</Button>
            <Button
              type="primary"
              onClick={() => setOpen(true)}
              data-testid="tokens-mint"
            >
              Mint token
            </Button>
          </Space>
        }
      >
        <Table
          rowKey="jti"
          loading={loading}
          dataSource={tokens}
          data-testid="tokens-table"
          columns={[
            { title: "Name", dataIndex: "name" },
            {
              title: "Scopes",
              dataIndex: "scopes",
              render: (s: string[]) =>
                (s ?? []).map((x) => (
                  <Tag key={x} color="blue">
                    {x}
                  </Tag>
                )),
            },
            { title: "Created", dataIndex: "created_at" },
            { title: "Expires", dataIndex: "expires_at" },
            { title: "Last used", dataIndex: "last_used_at",
              render: (v) => v ?? "—" },
            {
              title: "Status",
              dataIndex: "revoked",
              render: (v) =>
                v ? <Tag color="red">revoked</Tag> : <Tag color="green">active</Tag>,
            },
            {
              title: "Actions",
              render: (_, t: Token) => (
                <Space>
                  {t.revoked ? (
                    <Button
                      onClick={() => onReactivate(t)}
                      data-testid="token-reactivate"
                    >
                      Reactivate
                    </Button>
                  ) : (
                    <Button danger onClick={() => onRevoke(t)}>
                      Revoke
                    </Button>
                  )}
                  <Button
                    danger
                    onClick={() => onDelete(t)}
                    data-testid="token-delete"
                  >
                    Delete
                  </Button>
                </Space>
              ),
            },
          ]}
        />
      </Card>

      <Modal
        open={open}
        title="Mint token"
        onCancel={() => setOpen(false)}
        onOk={() => form.submit()}
        okText="Mint"
      >
        <Form form={form} layout="vertical" onFinish={onMint}>
          <Form.Item
            label="Name"
            name="name"
            rules={[{ required: true, message: "Required" }]}
            extra="Unique per user (e.g. ci-runner, laptop)."
          >
            <Input data-testid="mint-name" />
          </Form.Item>
          <Form.Item
            label="Scopes"
            name="scopes"
            initialValue={["mcp:read"]}
          >
            <Select
              mode="multiple"
              options={[
                { value: "mcp:read", label: "mcp:read" },
                { value: "mcp:write", label: "mcp:write" },
              ]}
              data-testid="mint-scopes"
            />
          </Form.Item>
          <Form.Item
            label="TTL (seconds)"
            name="ttl"
            initialValue={3600}
            rules={[{ required: true }]}
          >
            <InputNumber min={1} style={{ width: "100%" }} data-testid="mint-ttl" />
          </Form.Item>
        </Form>
      </Modal>

      <Modal
        open={mintedToken !== null}
        title="Copy this token now"
        destroyOnClose
        footer={[
          <Button
            key="copy"
            type="primary"
            onClick={() => {
              navigator.clipboard.writeText(mintedToken ?? "");
              message.success("Copied to clipboard");
            }}
            data-testid="minted-copy"
          >
            Copy
          </Button>,
          <Button
            key="close"
            onClick={() => setMintedToken(null)}
            data-testid="minted-close"
          >
            Close
          </Button>,
        ]}
        onCancel={() => setMintedToken(null)}
        maskClosable={false}
      >
        <Alert
          type="warning"
          showIcon
          message="This is the only time you will see the token. Store it somewhere safe."
        />
        <Typography.Paragraph
          copyable={{ text: mintedToken ?? "" }}
          code
          className="mt-4 break-all"
          data-testid="minted-value"
        >
          {mintedToken}
        </Typography.Paragraph>
      </Modal>
    </>
  );
}
